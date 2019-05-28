package usbt

sealed abstract class Scope extends Product with Serializable
case object This extends Scope
case object Global extends Scope
case object ThisBuild extends Scope

sealed abstract class Init[+A] {
  final def map[B](f: A => B): Init[B] = Init.Mapped(this, f)
}
object Init {
  final case class Value[A](value: A) extends Init[A]
  final case class Mapped[A, B](init: Init[A], f: A => B) extends Init[B]
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

  // flatMap
  // tuples
  // different key types
  // add tasks
  // add input tasks
  def main(args: Array[String]): Unit = {
    val baseDir = Key[String]("baseDir")
    val  srcDir = Key[String]( "srcDir")

    def pathAppend(a: String, b: String) = if (a.endsWith("/")) a + b else a + "/" + b

    val settingsSeq = Seq(
       srcDir in Global    <<= baseDir.map(pathAppend(_, "src")),
      baseDir in ThisBuild  := "/",
    )

    val settingsMap: Map[Name[String], Map[Scope, Init[String]]] = Map(
      baseDir.name -> Map(ThisBuild -> Init.Value("/")),
       srcDir.name -> Map(Global    -> baseDir.map(pathAppend(_, "src"))),
    )

    def check(key1: Key[String], expected: String) = {
      def lookup(key: Key[String]) = {
        val scopeMap = settingsMap(key.name)
        def log = sys.error(s"no ${keyToString(key)} in ${settingMapToString(settingsMap)}")
        key.scope match {
          case This      => scopeMap.getOrElse(if (key1.scope == This) Global else key1.scope, log)
          case Global    => scopeMap.getOrElse(Global, log)
          case ThisBuild => scopeMap.get(ThisBuild).getOrElse(scopeMap.getOrElse(Global, log))
        }
      }
      def evalInit(init: Init[String]): String = init match {
        case Init.Value(x: String)                                             => x
        case Init.Mapped(init: Init[String @unchecked], f: (String => String)) => f(evalInit(init))
        case key: Key[String]                                                  => evalInit(lookup(key))
      }
      val actual = evalInit(lookup(key1))
      if (actual != expected) println(s"Expected $expected, Actual $actual")
    }

    check(srcDir in ThisBuild, "/src")
  }
}
