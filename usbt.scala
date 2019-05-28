package usbt

import scala.collection.immutable
import scala.collection.mutable
import scala.collection.mutable.Builder

sealed abstract class Scope extends Product with Serializable {
  def thisFold[A](ifThis: A, ifNot: Scope => A): A = if (this == This) ifThis else ifNot(this)
  def resolveThis(scope: ResolvedScope): ResolvedScope = this match {
    case This            => scope
    case Global          => Global
    case ThisBuild       => ThisBuild
    case x: LocalProject => x
  }
}
case object This extends Scope
sealed trait ResolvedScope extends Scope
case object Global extends ResolvedScope
case object ThisBuild extends ResolvedScope
final case class LocalProject(id: String) extends ResolvedScope { override def toString = id }

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

final case class Name[A](value: String) { override def toString = value }

final case class Key[A](name: Name[A], scope: Scope) extends Init[A] {
  def in(scope: Scope): Key[A]       = Key(name, scope)
  def <<=(init: Init[A]): Setting[A] = Setting(this, init)
  def :=(value: A): Setting[A]       = this <<= Init.Value(value)
}

object Key {
  def apply[A](name: String): Key[A] = Key(Name(name), This)
}

final case class Setting[A](key: Key[A], init: Init[A]) {
  override def toString = {
    val op = if (init.isInstanceOf[Init[_]]) "  := " else " <<= "
    key + op + init
  }
}

final case class ScopeInitMap(scopeMap: Map[ResolvedScope, Init[_]]) {
  def get(scope: ResolvedScope, log: => Nothing): Init[_] = {
    def getGlobal    = scopeMap.getOrElse(Global, log)
    def getThisBuild = scopeMap.getOrElse(ThisBuild, getGlobal)
    scope match {
      case Global           => getGlobal
      case ThisBuild        => getThisBuild
      case LocalProject(id) => scopeMap.getOrElse(scope, getThisBuild)
    }
  }

  override def toString = {
    if (scopeMap.size <= 1) scopeMap.mkString else scopeMap.iterator
      .map { case (s, init) => "\n    " + s + " -> " + init }
      .mkString("[", "", "\n  ]")
  }
}

final class SettingMap(val settingsMap: scala.collection.Map[Name[_], ScopeInitMap]) {
  def getValue[A](key: Key[A]): A = evalInit(key, key.scope.resolveThis(Global))

  def getInit[A](key: Key[A], scope: ResolvedScope): Init[A] = {
    def log = sys.error(s"no ${Key(key.name, scope)} in $this")
    val init: Init[_] = settingsMap(key.name).get(scope, log)
    init.asInstanceOf[Init[A]] // guaranteed by SettingMap's builder's put signature
  }

  private def evalInit[A](init: Init[A], scope: ResolvedScope): A = init match {
    case Init.Value(x)         => x
    case Init.Mapped(init, f)  => f(evalInit(init, scope))
    case Init.ZipWith(a, b, f) => f(evalInit(a, scope), evalInit(b, scope))
    case Init.Bind(init, f)    => evalInit(f(evalInit(init, scope)), scope)
    case key: Key[A]           => evalInit(getInit(key, scope), scope)
  }

  override def toString = settingsMap.mkString("\nSettingMap [\n  ", "\n  ", "\n]")
}

object SettingMap {
  def newBuilder: Builder0 = new Builder0(mutable.LinkedHashMap.empty)

  final class Builder0(b: mutable.LinkedHashMap[Name[_], ScopeInitMap]) {
    def put[A](k: Key[A], v: Map[ResolvedScope, Init[A]]): Builder0 = { b.put(k.name, ScopeInitMap(v)); this }
    def result: SettingMap = new SettingMap(b)
  }
}

object Main {
  def groupByKey(ss: Seq[Setting[_]]) = {
    val zero = Map.empty[Key[_], Builder[Setting[_], Seq[Setting[_]]]]
    val map: Map[Key[_], Seq[Setting[_]]] = ss
        .foldLeft(zero)((acc, s) => acc.updated(s.key, acc.getOrElse(s.key, Seq.newBuilder) += s))
        .iterator
        .map { case (key, builder) => key -> builder.result() }
        .toMap
    println(map.mapValues(_.mkString("[\n    ", "\n    ", "\n  ]")).mkString("Map(\n  ", "\n  ", "\n)"))
  }

  implicit class StringWithSlash(private val self: String) extends AnyVal {
    def /(s: String) = if (self.endsWith("/")) self + s else self + "/" + s
  }

  // add tasks
  // add input tasks
  def main(args: Array[String]): Unit = {
    val        baseDir     = Key[String](       "baseDir")
    val         srcDir     = Key[String](        "srcDir")
    val      targetDir     = Key[String](     "targetDir")
    val    scalaSrcDir     = Key[String](   "scalaSrcDir")
    val        srcDirs     = Key[Seq[String]]("srcDirs")
    val crossTargetDir     = Key[String]("crossTargetDir")
    val scalaVersion       = Key[String]("scalaVersion")
    val scalaBinaryVersion = Key[String]("scalaBinaryVersion")

    val foo = LocalProject("foo")

    val settingsSeq = Seq(
                  srcDir in Global    <<= baseDir.map(_ / "src"),
               targetDir in Global    <<= baseDir.map(_ / "target"),
             scalaSrcDir in Global    <<= srcDir.map(_ / "main/scala"),
                 srcDirs in Global    <<= scalaSrcDir.zipWith(scalaBinaryVersion)((dir, sbv) => Seq(dir, s"$dir-$sbv")),
          crossTargetDir in Global    <<= targetDir.zipWith(scalaBinaryVersion)((target, sbv) => target / s"scala-$sbv"),
                 baseDir in ThisBuild  := "/",
      scalaVersion       in ThisBuild  := "2.12.8",
      scalaBinaryVersion in ThisBuild  := "2.12",
                 baseDir in foo        := "/foo",
    )

    val settingsMap: SettingMap = SettingMap
        .newBuilder
        .put(            srcDir, Map(Global    -> baseDir.map(_ / "src")))
        .put(         targetDir, Map(Global    -> baseDir.map(_ / "target")))
        .put(       scalaSrcDir, Map(Global    -> srcDir.map(_ / "main/scala")))
        .put(           srcDirs, Map(Global    -> scalaSrcDir.zipWith(scalaBinaryVersion)((dir, sbv) => Seq(dir, s"$dir-$sbv"))))
        .put(    crossTargetDir, Map(Global    -> targetDir.zipWith(scalaBinaryVersion)((target, sbv) => target / s"scala-$sbv")))
        .put(           baseDir, Map(ThisBuild -> Init.Value("/"), foo -> Init.Value("/foo")))
        .put(      scalaVersion, Map(ThisBuild -> Init.Value("2.12.8")))
        .put(scalaBinaryVersion, Map(ThisBuild -> Init.Value("2.12")))
        .result

    println(settingsMap)

    def check[A](key: Key[A], expected: A) = {
      val actual = settingsMap.getValue(key)
      if (actual != expected) println(s"Expected $expected, Actual $actual")
    }

    check(srcDir in ThisBuild,    "/src")
    check(srcDir in foo,          "/foo/src")

    check(targetDir in ThisBuild, "/target")
    check(targetDir in foo,       "/foo/target")

    check(scalaVersion in foo, "2.12.8")
    check(scalaBinaryVersion in foo, "2.12")

    check(   scalaSrcDir in foo, "/foo/src/main/scala")
    check(       srcDirs in foo, Seq("/foo/src/main/scala", "/foo/src/main/scala-2.12"))
    check(crossTargetDir in foo, "/foo/target/scala-2.12")
  }
}
