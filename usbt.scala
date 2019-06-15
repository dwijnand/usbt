package usbt

import scala.collection.immutable.ListMap
import scala.collection.mutable.{ Builder, LinkedHashMap }

object Types {
  type AnyName    = Name[_]
  type AnyInit    = Init[_]
  type AnySetting = Setting[_]

  type Id[X] = X

  trait ~>[-A[_], +B[_]] {
    def apply[T](a: A[T]): B[T]
  }

  sealed trait T2K[A, B] { type l[L[x]] = (L[A], L[B]) }
}
import Types._

trait AList[K[M[x]]] {
  def transform[M[_], N[_]](value: K[M], f: M ~> N): K[N]
}

object AList {
  type T2List[A, B] = AList[T2K[A, B]#l]
  def tuple2[A, B]: T2List[A, B] = new AList[T2K[A, B]#l] {
    type T2[M[_]] = (M[A], M[B])
    def transform[M[_], N[_]](t: T2[M], f: M ~> N): T2[N] = (f(t._1), f(t._2))
  }
}

final case class Name[A](value: String) { override def toString = value }

sealed abstract class Scope extends Product with Serializable {
  def thisFold[A](ifThis: A, ifNot: Scope => A): A = if (this == This) ifThis else ifNot(this)
  def or(scope: ResolvedScope): ResolvedScope = this match {
    case This            => scope
    case Global          => Global
    case ThisBuild       => ThisBuild
    case x: LocalProject => x
  }
  def orGlobal = or(Global)
}
case object This extends Scope
sealed trait ResolvedScope extends Scope
case object Global extends ResolvedScope
case object ThisBuild extends ResolvedScope
final case class LocalProject(id: String) extends ResolvedScope { override def toString = id }

final case class Key[A](name: Name[A], scope: Scope) extends Init[A] {
  def in(scope: Scope): Key[A]       = Key(name, scope)
  def <<=(init: Init[A]): Setting[A] = Setting(this, init)
  def :=(value: A): Setting[A]       = this <<= Init.Value(value)
}

object Key {
  def apply[A](name: String): Key[A] = Key(Name(name), This)
}

sealed abstract class Init[+A] extends Product with Serializable {
  final def map[B](f: A => B): Init[B]                         = Init.Mapped(this, f)
  final def zipWith[B, C](x: Init[B])(f: (A, B) => C): Init[C] = Init.ZipWith[T2K[A, B]#l, C]((this, x), f.tupled, AList.tuple2)
  final def flatMap[B](f: A => Init[B]): Init[B]               = Init.Bind(this, f)

  final def zip[B](x: Init[B]): Init[(A, B)] = zipWith(x)((_, _))

  final override def toString = this match {
    case Init.Value(x)             => if (x.isInstanceOf[String]) s""""$x"""" else s"$x"
    case Init.Mapped(init, _)      => s"$init.map(<f>)"
    case Init.ZipWith(inits, _, _) => s"Init.zipWith($inits)(<f>)"
    case Init.Bind(init, _)        => s"$init.flatMap(<f>)"
    case Key(name, scope)          => scope.thisFold(s"$name", scope => s"$scope / $name")
  }
}
object Init {
  final case class Value[A](value: A)                                                  extends Init[A]
  final case class Mapped[A, B](init: Init[A], f: A => B)                              extends Init[B]
  final case class ZipWith[K[M[x]], A](inits: K[Init], f: K[Id] => A, alist: AList[K]) extends Init[A]
  final case class Bind[A, B](init: Init[A], f: A => Init[B])                          extends Init[B]
}

final case class Setting[A](key: Key[A], init: Init[A]) {
  override def toString = key + (if (init.isInstanceOf[AnyInit]) "  := " else " <<= ") + init
}

/** `Name -> Scope -> Init`, for example:
 *  baseDir -> Global -> Value(/)
 *  baseDir ->  foo   -> Value(/foo)
 */
final case class ScopeInitMap(underlying: ListMap[ResolvedScope, AnyInit]) {
  def getOrElse(scope: ResolvedScope, default: => AnyInit): AnyInit = {
    def getGlobal    = underlying.getOrElse(Global, default)
    def getThisBuild = underlying.getOrElse(ThisBuild, getGlobal)
    scope match {
      case Global           => getGlobal
      case ThisBuild        => getThisBuild
      case LocalProject(id) => underlying.getOrElse(scope, getThisBuild)
    }
  }

  override def toString = if (underlying.size <= 1) underlying.mkString else underlying.mkString("[ ", ", ", " ]")
}

object ScopeInitMap {
  val empty = ScopeInitMap(ListMap.empty)
}

final case class SettingMap private (underlying: ListMap[AnyName, ScopeInitMap]) {
  def getValue[A](key: Key[A]): A = evalInit(key, key.scope.orGlobal)

  def getInit[A](key: Key[A], scope: ResolvedScope): Init[A] = {
    val res = underlying
        .getOrElse(key.name, ScopeInitMap.empty)
        .getOrElse(scope, sys.error(s"no $scope / ${key.name} in $this"))
        .asInstanceOf[Init[A]] // guaranteed by SettingMap's builder's put signature
//    println(s"getInit($key, $scope) = $res")
    res
  }

  private def evalInit[A](init: Init[A], scope: ResolvedScope): A = {
//    println(s"evalInit($init, $scope)")
    val eval = new ~>[Init, Id] { eval =>
      def apply[T](x: Init[T]): T = x match {
        case Init.Value(x)                 => x
        case Init.Mapped(init, f)          => f(eval(init))
        case Init.ZipWith(inits, f, alist) => f(alist.transform(inits, eval))
        case Init.Bind(init, f)            => eval(f(eval(init)))
        case key: Key[T]                   => eval(getInit(key, key.scope.or(scope)))
      }
    }
    val res = eval(init)
//    println(s"evalInit($init, $scope) = $res")
    res
  }

  override def toString = underlying.mkString("SettingMap [\n  ", "\n  ", "\n]")
}

object SettingMap {
  def fromVarargs(ss: AnySetting*): SettingMap = ss.foldLeft(newBuilder)(_.put(_)).result
  def newBuilder: Builder0 = new Builder0(LinkedHashMap.empty)

  type MapBuilder[K, V, M[_, _]] = Builder[(K, V), M[K, V]]

  final class Builder0(b: LinkedHashMap[AnyName, MapBuilder[ResolvedScope, AnyInit, ListMap]]) {
    def put[A](s: Setting[A]): Builder0 = {
      b.getOrElseUpdate(s.key.name, ListMap.newBuilder) += s.key.scope.orGlobal -> s.init
      this
    }
    def result: SettingMap = {
      val settingsMap = b.iterator.foldLeft(ListMap.newBuilder[AnyName, ScopeInitMap]) {
        case (acc, (name, b)) => acc += name -> ScopeInitMap(b.result)
      }.result
      new SettingMap(settingsMap)
    }
  }
}

object Main {
  implicit class StringWithSlash(private val self: String) extends AnyVal {
    def /(s: String) = if (self.endsWith("/")) self + s else self + "/" + s
  }

  // classpathOptions (in console) https://github.com/lampepfl/dotty/pull/6577/files
  // Compile/Test vs console scopes
  // alternative https://www.scala-sbt.org/1.x/docs/Scope-Delegation.html
  // add tasks
  // add input tasks
  def main(args: Array[String]): Unit = {
    val bippy = LocalProject("bippy")

    val foo = Key[String]("foo")
    val bar = Key[String]("bar")
    val baz = Key[String]("baz")

    def assertEquals[A](actual: A, expected: A, desc: String = "") = {
      if (actual != expected)
        if (desc == "") println(s"Expected $expected, Actual $actual") else
          println(s"For $desc: Expected $expected, Actual $actual")
    }

    def assertSettings[A](settingsMap: SettingMap)(ss: AnySetting*) = {
      println(settingsMap)
      ss.foreach(x => (x: @unchecked) match { case Setting(key, Init.Value(value)) =>
        assertEquals(settingsMap.getValue(key), value, key.toString)
      })
    }

    def ignore(x: => Any) = ()

    {
      val        baseDir     = Key[String](       "baseDir")
      val         srcDir     = Key[String](        "srcDir")
      val      targetDir     = Key[String](     "targetDir")
      val    scalaSrcDir     = Key[String](   "scalaSrcDir")
      val        srcDirs     = Key[Seq[String]]("srcDirs")
      val crossTargetDir     = Key[String]("crossTargetDir")
      val scalaVersion       = Key[String]("scalaVersion")
      val scalaBinaryVersion = Key[String]("scalaBinaryVersion")

      val settingsMap: SettingMap = SettingMap.fromVarargs(
                    srcDir in Global    <<= baseDir.map(_ / "src"),
                 targetDir in Global    <<= baseDir.map(_ / "target"),
               scalaSrcDir in Global    <<= srcDir.map(_ / "main/scala"),
                   srcDirs in Global    <<= scalaSrcDir.zipWith(scalaBinaryVersion)((dir, sbv) => Seq(dir, s"$dir-$sbv")),
            crossTargetDir in Global    <<= targetDir.zipWith(scalaBinaryVersion)((target, sbv) => target / s"scala-$sbv"),
                   baseDir in ThisBuild  := "/",
        scalaVersion       in ThisBuild  := "2.12.8",
        scalaBinaryVersion in ThisBuild  := "2.12",
                   baseDir in bippy      := "/bippy",
      )

      assertSettings(settingsMap)(
        srcDir in ThisBuild    := "/src",
        srcDir in bippy        := "/bippy/src",

        targetDir in ThisBuild := "/target",
        targetDir in bippy     := "/bippy/target",

              scalaVersion in bippy := "2.12.8",
        scalaBinaryVersion in bippy := "2.12",

           scalaSrcDir in bippy := "/bippy/src/main/scala",
               srcDirs in bippy := Seq("/bippy/src/main/scala", "/bippy/src/main/scala-2.12"),
        crossTargetDir in bippy := "/bippy/target/scala-2.12",
      )
    }

    assertSettings(SettingMap.fromVarargs(
      foo in Global  := "g",
      foo in bippy   := "b",
      bar in bippy  <<= foo,
      baz in bippy  <<= foo in Global,
    ))(
      bar in bippy := "b",
      baz in bippy := "g",
    )
  }
}
