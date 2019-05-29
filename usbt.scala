package usbt

import scala.collection.immutable.ListMap
import scala.collection.mutable.{ Builder, LinkedHashMap }

object Aliases {
  type AnyName    = Name[_]
  type AnyKey     = Key[_]
  type AnyInit    = Init[_]
  type AnySetting = Setting[_]
}
import Aliases._

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
  final def zipWith[B, C](x: Init[B])(f: (A, B) => C): Init[C] = Init.ZipWith(this, x, f)
  final def flatMap[B](f: A => Init[B]): Init[B]               = Init.Bind(this, f)

  final override def toString = this match {
    case Init.Value(x)         => if (x.isInstanceOf[String]) s""""$x"""" else s"$x"
    case Init.Mapped(init, _)  => s"$init.map(<f>)"
    case Init.ZipWith(a, b, _) => s"$a.zipWith($b)(<f>)"
    case Init.Bind(init, _)    => s"$init.flatMap(<f>)"
    case Key(name, scope)      => scope.thisFold(s"$name", scope => s"$scope / $name")
  }
}
object Init {
  final case class Value[A](value: A)                                       extends Init[A]
  final case class Mapped[A, B](init: Init[A], f: A => B)                   extends Init[B]
  final case class ZipWith[A, B, C](a: Init[A], b: Init[B], f: (A, B) => C) extends Init[C]
  final case class Bind[A, B](init: Init[A], f: A => Init[B])               extends Init[B]
}

final case class Setting[A](key: Key[A], init: Init[A]) {
  override def toString = key + (if (init.isInstanceOf[AnyInit]) "  := " else " <<= ") + init
}

final case class ScopeInitMap(self: ListMap[ResolvedScope, AnyInit]) {
  def getOrElse(scope: ResolvedScope, default: => AnyInit): AnyInit = {
    def getGlobal    = self.getOrElse(Global, default)
    def getThisBuild = self.getOrElse(ThisBuild, getGlobal)
    scope match {
      case Global           => getGlobal
      case ThisBuild        => getThisBuild
      case LocalProject(id) => self.getOrElse(scope, getThisBuild)
    }
  }

  override def toString = if (self.size <= 1) self.mkString else self.mkString("[ ", ", ", " ]")
}

object ScopeInitMap {
  val empty = ScopeInitMap(ListMap.empty)
}

final case class SettingMap private (self: ListMap[AnyName, ScopeInitMap]) {
  def getValue[A](key: Key[A]): A = evalInit(key, key.scope.orGlobal)

  def getInit[A](key: Key[A], scope: ResolvedScope): Init[A] = {
    val res = self
        .getOrElse(key.name, ScopeInitMap.empty)
        .getOrElse(scope, sys.error(s"no $scope / ${key.name} in $this"))
        .asInstanceOf[Init[A]] // guaranteed by SettingMap's builder's put signature
//    println(s"getInit($key, $scope) = $res")
    res
  }

  private def evalInit[A](init: Init[A], scope: ResolvedScope): A = {
//    println(s"evalInit($init, $scope)")
    val res = init match {
      case Init.Value(x)         => x
      case Init.Mapped(init, f)  => f(evalInit(init, scope))
      case Init.ZipWith(a, b, f) => f(evalInit(a, scope), evalInit(b, scope))
      case Init.Bind(init, f)    => evalInit(f(evalInit(init, scope)), scope)
      case key: Key[A]           => evalInit(getInit(key, key.scope.or(scope)), key.scope.or(scope))
    }
//    println(s"evalInit($init, $scope) = $res")
    res
  }

  override def toString = self.mkString("SettingMap [\n  ", "\n  ", "\n]")
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
      // println(settingsMap)
      ss.foreach { case Setting(key, Init.Value(value)) =>
        assertEquals(settingsMap.getValue(key), value, key.toString)
      }
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
