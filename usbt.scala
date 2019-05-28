package usbt

import scala.collection.GenTraversableOnce
import scala.collection.immutable
import scala.collection.mutable
import scala.collection.mutable.Builder

final class Formatter(wr: java.io.BufferedWriter, var level: Int = 0, var dirty: Boolean = false) {
  def indented[A](start: String, xs: GenTraversableOnce[A], end: String)(op: A => Unit) = {
    println(start)
    level += 1
    xs.foreach(op)
    level -= 1
    println(end)
  }
  def newLine               = { wr.newLine(); dirty = false }
  def print(s: String)      = { if (!dirty) wr.write("  " * level); wr.write(s); dirty = true }
  def println(s: String)    = { print(s); newLine }
}

trait Display {
  def fmt(f: Formatter): Unit
}

trait SelfDisplay extends Display {
  final override def toString = {
    val sw = new java.io.StringWriter()
    val bw = new java.io.BufferedWriter(sw)
    fmt(new Formatter(bw))
    bw.flush()
    sw.toString
  }
}

sealed abstract class Scope extends Product with Serializable with SelfDisplay {
  def fmt(f: Formatter): Unit = this match {
    case This             => f.print("This")
    case Global           => f.print("Global")
    case ThisBuild        => f.print("ThisBuild")
    case LocalProject(id) => f.print(id)
  }
}
case object This extends Scope
case object Global extends Scope
case object ThisBuild extends Scope
final case class LocalProject(id: String) extends Scope

sealed abstract class Init[+A] extends SelfDisplay {
  final def map[B](f: A => B): Init[B]                         = Init.Mapped(this, f)
  final def flatMap[B](f: A => Init[B]): Init[B]               = Init.Bind(this, f)
  final def zipWith[B, C](x: Init[B])(f: (A, B) => C): Init[C] = flatMap(a => x.map(b => f(a, b)))

  def fmt(f: Formatter) = Init.toString(f, this)
}
object Init {
  final case class Value[A](value: A) extends Init[A]
  final case class Mapped[A, B](init: Init[A], f: A => B) extends Init[B]
  final case class Bind[A, B](init: Init[A], f: A => Init[B]) extends Init[B]

  def toString(f: Formatter, init: Init[_], addOp: Boolean = false) = init match {
    case Init.Value(x)        => if (addOp) f.print("  := "); f.print(anyToString(x))
    case Init.Mapped(init, _) => if (addOp) f.print(" <<= "); init.fmt(f); f.print(".map(<f>)")
    case Init.Bind(init, _)   => if (addOp) f.print(" <<= "); init.fmt(f); f.print(".flatMap(<f>)")
    case key: Key[_]          =>
      if (addOp) f.print(" <<= ")
      if (key.scope != This) {
        key.scope.fmt(f)
        f.print(" / ")
      }
      f.print(key.name.value)
  }

  private def anyToString(x: Any) = x match {
    case s: String => s""""$s""""
    case _         => s"$x"
  }
}

final case class Name[A](value: String)

final case class Key[A](name: Name[A], scope: Scope) extends Init[A] {
  def in(scope: Scope): Key[A]       = Key(name, scope)
  def <<=(init: Init[A]): Setting[A] = Setting(this, init)
  def :=(value: A): Setting[A]       = this <<= Init.Value(value)
}

object Key {
  def apply[A](name: String): Key[A] = Key(Name(name), This)
}

final case class Setting[A](key: Key[A], init: Init[A]) extends SelfDisplay {
  def fmt(f: Formatter) = { key.fmt(f); init.fmt(f) }
}

final case class ScopeInitMap(scopeMap: Map[Scope, Init[_]]) extends SelfDisplay {
  def get(scope: Scope, log: => Nothing): Init[_] = {
    def getGlobal    = scopeMap.getOrElse(Global, log)
    def getThisBuild = scopeMap.getOrElse(ThisBuild, getGlobal)
    scope match {
      case This             => getGlobal
      case Global           => getGlobal
      case ThisBuild        => getThisBuild
      case LocalProject(id) => scopeMap.getOrElse(scope, getThisBuild)
    }
  }

  def fmt(f: Formatter) = {
    if (scopeMap.size <= 1) f.print(scopeMap.mkString) else {
      f.indented("[", scopeMap, "]") { case (scope, init) =>
        scope.fmt(f)
        f.print(" -> ")
        init.fmt(f)
        f.newLine
      }
    }
    f.newLine
  }
}

final class SettingMap(val settingsMap: scala.collection.Map[Name[_], ScopeInitMap]) extends SelfDisplay {
  import Main._

  def getValue[A](key: Key[A]): A = evalInit(getInit(key, key.scope), key.scope)

  def getInit[A](key: Key[A], scope0: Scope): Init[A] = {
    val scope = if (key.scope == This) scope0 else key.scope
    def log = sys.error(s"no ${Key(key.name, scope)} in $this")
    val init: Init[_] = settingsMap(key.name).get(scope, log)
    init.asInstanceOf[Init[A]] // guaranteed by SettingMap's builder's put signature
  }

  private def evalInit[A](init: Init[A], scope: Scope): A = init match {
    case Init.Value(x: A @unchecked)                            => x
    case Init.Mapped(init: Init[A @unchecked], f: (A => A))     => f(evalInit(init, scope))
    case Init.Bind(init: Init[A @unchecked], f: (A => Init[A])) => evalInit(f(evalInit(init, scope)), scope)
    case key: Key[A]                                            => evalInit(getInit(key, scope), scope)
  }

  def fmt(f: Formatter) = {
    f.indented("\nSettingMap [", settingsMap, "]") { case (name, scopeInitMap) =>
      f.print(s"${name.value} -> ")
      scopeInitMap.fmt(f)
    }
  }
}

object SettingMap {
  def newBuilder: Builder0 = new Builder0(mutable.LinkedHashMap.empty)

  final class Builder0(b: mutable.LinkedHashMap[Name[_], ScopeInitMap]) {
    def put[A](k: Key[A], v: Map[Scope, Init[A]]): Builder0 = { b.put(k.name, ScopeInitMap(v)); this }
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
        .put(           baseDir, Map(ThisBuild -> Init.Value("/"), foo -> Init.Value("/foo")))
        .put(            srcDir, Map(Global    -> baseDir.map(_ / "src")))
        .put(         targetDir, Map(Global    -> baseDir.map(_ / "target")))
        .put(       scalaSrcDir, Map(Global    -> srcDir.map(_ / "main/scala")))
        .put(           srcDirs, Map(Global    -> scalaSrcDir.zipWith(scalaBinaryVersion)((dir, sbv) => Seq(dir, s"$dir-$sbv"))))
        .put(    crossTargetDir, Map(Global    -> targetDir.zipWith(scalaBinaryVersion)((target, sbv) => target / s"scala-$sbv")))
        .put(      scalaVersion, Map(ThisBuild -> Init.Value("2.12.8")))
        .put(scalaBinaryVersion, Map(ThisBuild -> Init.Value("2.12")))
        .result

    println(settingsMap)

    def check[A](key: Key[A], expected: A) = {
//      val actual = settingsMap.getValue(key)
//      if (actual != expected) println(s"Expected $expected, Actual $actual")
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
