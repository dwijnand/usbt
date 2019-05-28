package usbt

import scala.collection.mutable
import scala.collection.mutable.Builder

sealed abstract class Scope extends Product with Serializable
case object This extends Scope
case object Global extends Scope
case object ThisBuild extends Scope
final case class LocalProject(id: String) extends Scope

sealed abstract class Init[+A] {
  final def map[B](f: A => B): Init[B]                         = Init.Mapped(this, f)
  final def flatMap[B](f: A => Init[B]): Init[B]               = Init.Bind(this, f)
  final def zipWith[B, C](x: Init[B])(f: (A, B) => C): Init[C] = flatMap(a => x.map(b => f(a, b)))
}
object Init {
  final case class Value[A](value: A) extends Init[A]
  final case class Mapped[A, B](init: Init[A], f: A => B) extends Init[B]
  final case class Bind[A, B](init: Init[A], f: A => Init[B]) extends Init[B]
}

final case class Name[A](value: String)

final case class Key[A](name: Name[A], scope: Scope) extends Init[A] {
  final def in(scope: Scope): Key[A]       = Key(name, scope)
  final def <<=(init: Init[A]): Setting[A] = Setting(this, init)
  final def :=(value: A): Setting[A]       = this <<= Init.Value(value)
}

object Key {
  def apply[A](name: String): Key[A] = Key(Name(name), This)
}

final case class Setting[A](key: Key[A], init: Init[A])

final class SettingMap(val settingsMap: Map[Name[_], Map[Scope, Init[_]]]) {
  import Main._

  def getValue[A](key: Key[A]): A = evalInit(getInit(key, key.scope), key.scope)

  def getInit[A](key: Key[A], scope0: Scope): Init[A] = {
    val scope = if (key.scope == This) scope0 else key.scope
    val scopeMap = settingsMap(key.name)
    def log = sys.error(s"no ${keyToString(Key(key.name, scope))} in ${settingMapToString(this)}")
    def getGlobal    = scopeMap.getOrElse(Global, log)
    def getThisBuild = scopeMap.getOrElse(ThisBuild, getGlobal)
    val init: Init[_] = scope match {
      case This             => getGlobal
      case Global           => getGlobal
      case ThisBuild        => getThisBuild
      case LocalProject(id) => scopeMap.getOrElse(scope, getThisBuild)
    }
    init.asInstanceOf[Init[A]]
  }

  private def evalInit[A](init: Init[A], scope: Scope): A = init match {
    case Init.Value(x: A @unchecked)                            => x
    case Init.Mapped(init: Init[A @unchecked], f: (A => A))     => f(evalInit(init, scope))
    case Init.Bind(init: Init[A @unchecked], f: (A => Init[A])) => evalInit(f(evalInit(init, scope)), scope)
    case key: Key[A]                                            => evalInit(getInit(key, scope), scope)
  }
}

object SettingMap {
  def newBuilder: Builder0 = new Builder0(mutable.HashMap.empty)

  final class Builder0(b: mutable.HashMap[Name[_], Map[Scope, Init[_]]]) {
    def put[A](k: Key[A], v: Map[Scope, Init[A]]): Builder0 = { b.put(k.name, v); this }
    def result: SettingMap = new SettingMap(b.toMap)
  }
}

object Main {

  def anyToString(x: Any) = x match {
    case s: String => s""""$s""""
    case _         => s"$x"
  }

  def keyToString(key: Key[_]) = s"""${key.scope} / ${key.name.value}"""

  def initToString(init: Init[_], addOp: Boolean = false): String = init match {
    case Init.Value(x)        => (if (addOp) "  := " else "") + anyToString(x)
    case Init.Mapped(init, _) => (if (addOp) " <<= " else "") + initToString(init) + ".map(<f>)"
    case Init.Bind(init, _)   => (if (addOp) " <<= " else "") + initToString(init) + ".flatMap(<f>)"
    case key: Key[_]          => (if (addOp) " <<= " else "") + keyToString(key)
  }

  def settingToString(s: Setting[_]) = keyToString(s.key) + initToString(s.init, addOp = true)

  def groupByKey(ss: Seq[Setting[_]]) = {
    val zero = Map.empty[Key[_], Builder[Setting[_], Seq[Setting[_]]]]
    val map: Map[Key[_], Seq[Setting[_]]] = ss
        .foldLeft(zero)((acc, s) => acc.updated(s.key, acc.getOrElse(s.key, Seq.newBuilder) += s))
        .iterator
        .map { case (key, builder) => key -> builder.result() }
        .toMap
    println(map.map(kv => keyToString(kv._1) -> kv._2.map(settingToString).mkString("[\n    ", "\n    ", "\n  ]")).mkString("Map(\n  ", "\n  ", "\n)"))
  }

  def printSettingSeq(ss: Seq[Setting[_]]) = {
    println(ss.map(settingToString).mkString("[\n  ", "\n  ", "\n]"))
  }

  def settingMapToString(map: SettingMap) = {
    map.settingsMap.map(kv => kv._1.value -> kv._2.map(kv => kv._1 -> initToString(kv._2)).mkString("Map(\n    ", "\n    ", "\n  )")).mkString("\nMap(\n  ", "\n  ", "\n)")
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

    def pathAppend(a: String, b: String) = if (a.endsWith("/")) a + b else a + "/" + b

    val settingsSeq = Seq(
                  srcDir in Global    <<= baseDir.map(pathAppend(_, "src")),
               targetDir in Global    <<= baseDir.map(pathAppend(_, "target")),
             scalaSrcDir in Global    <<= srcDir.map(pathAppend(_, "main/scala")),
                 srcDirs in Global    <<= scalaSrcDir.zipWith(scalaBinaryVersion)((dir, sbv) => Seq(dir, s"$dir-$sbv")),
          crossTargetDir in Global    <<= targetDir.zipWith(scalaBinaryVersion)((target, sbv) => pathAppend(target, s"scala-$sbv")),
                 baseDir in ThisBuild  := "/",
      scalaVersion       in ThisBuild  := "2.12.8",
      scalaBinaryVersion in ThisBuild  := "2.12",
                 baseDir in foo        := "/foo",
    )

    val settingsMap: SettingMap = SettingMap
        .newBuilder
        .put(           baseDir, Map(ThisBuild -> Init.Value("/"), foo -> Init.Value("/foo")))
        .put(            srcDir, Map(Global    -> baseDir.map(pathAppend(_, "src"))))
        .put(         targetDir, Map(Global    -> baseDir.map(pathAppend(_, "target"))))
        .put(       scalaSrcDir, Map(Global    -> srcDir.map(pathAppend(_, "main/scala"))))
        .put(           srcDirs, Map(Global    -> scalaSrcDir.zipWith(scalaBinaryVersion)((dir, sbv) => Seq(dir, s"$dir-$sbv"))))
        .put(    crossTargetDir, Map(Global    -> targetDir.zipWith(scalaBinaryVersion)((target, sbv) => pathAppend(target, s"scala-$sbv"))))
        .put(      scalaVersion, Map(ThisBuild -> Init.Value("2.12.8")))
        .put(scalaBinaryVersion, Map(ThisBuild -> Init.Value("2.12")))
        .result

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
