package usbt

sealed abstract class Scope
case object This extends Scope
case object Global extends Scope
case object ThisBuild extends Scope

sealed abstract class Init[A] {
  final def map[B](f: A => B): Init[B] = Init.Mapped(this, f)
}
object Init {
  final case class Value[A](value: A) extends Init[A]
  final case class Mapped[A, B](init: Init[A], f: A => B) extends Init[B]
}

final case class Key[A](name: String, scope: Scope = This) extends Init[A] {
  final def in(scope: Scope): Key[A]       = Key(name, scope)
  final def <<=(init: Init[A]): Setting[A] = Setting(this, init)
  final def :=(value: A): Setting[A]       = this <<= Init.Value(value)
}

final case class Setting[A](key: Key[A], init: Init[A])

object Main {
  def main(args: Array[String]): Unit = {
    val baseDir = Key[String]("baseDir")
    val  srcDir = Key[String]( "srcDir")

    val settings = Seq(
       srcDir in Global    <<= baseDir.map(_ + "/src"),
      baseDir in ThisBuild  := "/",
    )

    println(settings.map(settingToString).mkString("[\n  ", "\n  ", "\n]"))

    def check[A](key: Key[A], expected: A) = {
      val actual = null.asInstanceOf[A]
      if (actual != expected) println(s"Expected $expected, Actual $actual")
    }

    check(srcDir in ThisBuild, "/src")
  }

  def settingToString(s: Setting[_]) = keyToString(s.key) + initToString(s.init, addOp = true)

  def keyToString(key: Key[_]) = s"""${key.scope} / ${key.name}"""

  def initToString(init: Init[_], addOp: Boolean = false): String = init match {
    case Init.Value(x)        => (if (addOp) "  := " else "") + anyToString(x)
    case Init.Mapped(init, _) => (if (addOp) " <<= " else "") + initToString(init) + ".map(<f>)"
    case key: Key[_]          => (if (addOp) " <<= " else "") + keyToString(key)
  }

  def anyToString(x: Any) = x match {
    case s: String => s""""$s""""
    case _         => s"$x"
  }
}
