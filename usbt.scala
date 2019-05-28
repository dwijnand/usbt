package usbt

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
    import scala.collection.mutable.Builder
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

  def settingMapToString(map: Map[Name[String], Map[Scope, Init[String]]]) = {
    map.map(kv => kv._1.value -> kv._2.map(kv => kv._1 -> initToString(kv._2)).mkString("Map(\n    ", "\n    ", "\n  )")).mkString("\nMap(\n  ", "\n  ", "\n)")
  }

  // different key types
  // add tasks
  // add input tasks
  def main(args: Array[String]): Unit = {
    val        baseDir     = Key[String](       "baseDir")
    val         srcDir     = Key[String](        "srcDir")
    val    scalaSrcDir     = Key[String](   "scalaSrcDir")
    val      targetDir     = Key[String](     "targetDir")
    val crossTargetDir     = Key[String]("crossTargetDir")
    val scalaVersion       = Key[String]("scalaVersion")
    val scalaBinaryVersion = Key[String]("scalaBinaryVersion")

    val foo = LocalProject("foo")

    def pathAppend(a: String, b: String) = if (a.endsWith("/")) a + b else a + "/" + b

    val settingsSeq = Seq(
                  srcDir in Global    <<= baseDir.map(pathAppend(_, "src")),
               targetDir in Global    <<= baseDir.map(pathAppend(_, "target")),
             scalaSrcDir in Global    <<= srcDir.map(pathAppend(_, "main/scala")),
          crossTargetDir in Global    <<= targetDir.zipWith(scalaBinaryVersion)((target, sbv) => pathAppend(target, s"scala-$sbv")),
                 baseDir in ThisBuild  := "/",
      scalaVersion       in ThisBuild  := "2.12.8",
      scalaBinaryVersion in ThisBuild  := "2.12",
                 baseDir in foo        := "/foo",
    )

    val settingsMap: Map[Name[String], Map[Scope, Init[String]]] = Map(
      baseDir.name -> Map(
        ThisBuild -> Init.Value("/"),
              foo -> Init.Value("/foo"),
      ),
              srcDir.name -> Map(Global -> baseDir.map(pathAppend(_, "src"))),
           targetDir.name -> Map(Global -> baseDir.map(pathAppend(_, "target"))),
         scalaSrcDir.name -> Map(Global -> srcDir.map(pathAppend(_, "main/scala"))),
      crossTargetDir.name -> Map(Global -> targetDir.zipWith(scalaBinaryVersion)((target, sbv) => pathAppend(target, s"scala-$sbv"))),
      scalaVersion.name -> Map(ThisBuild -> Init.Value("2.12.8")),
      scalaBinaryVersion.name -> Map(ThisBuild -> Init.Value("2.12")),
    )

    def check(key: Key[String], expected: String) = {
      def getInit(key: Key[String], scope0: Scope): Init[String] = {
        val scope = if (key.scope == This) scope0 else key.scope
        val scopeMap = settingsMap(key.name)
        def log = sys.error(s"no ${keyToString(Key(key.name, scope))} in ${settingMapToString(settingsMap)}")
        def getGlobal    = scopeMap.getOrElse(Global, log)
        def getThisBuild = scopeMap.getOrElse(ThisBuild, getGlobal)
        scope match {
          case This             => getGlobal
          case Global           => getGlobal
          case ThisBuild        => getThisBuild
          case LocalProject(id) => scopeMap.getOrElse(scope, getThisBuild)
        }
      }
      def evalInit(init: Init[String], scope: Scope): String = init match {
        case Init.Value(x: String)                                                 => x
        case Init.Mapped(init: Init[String @unchecked], f: (String => String))     => f(evalInit(init, scope))
        case Init.Bind(init: Init[String @unchecked], f: (String => Init[String])) => evalInit(f(evalInit(init, scope)), scope)
        case key: Key[String]                                                      => evalInit(getInit(key, scope), scope)
      }
      val actual = evalInit(getInit(key, key.scope), key.scope)
      if (actual != expected) println(s"Expected $expected, Actual $actual")
    }

    check(srcDir in ThisBuild,    "/src")
    check(srcDir in foo,          "/foo/src")

    check(targetDir in ThisBuild, "/target")
    check(targetDir in foo,       "/foo/target")

    check(scalaVersion in foo, "2.12.8")
    check(scalaBinaryVersion in foo, "2.12")

    check(   scalaSrcDir in foo, "/foo/src/main/scala")
    check(crossTargetDir in foo, "/foo/target/scala-2.12")
  }
}
