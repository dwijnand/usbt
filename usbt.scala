package usbt

sealed abstract class Scope
case object This extends Scope
case object Global extends Scope
case object ThisBuild extends Scope

final case class AttrKey[A](name: String)

sealed trait ScopedKey[A] {
  def scope: Scope
  def attrKey: AttrKey[A]
}

sealed abstract class Init[A] {
  final def map[B](f: A => B): Init[B] = Init.Map(this, f)
}
object Init {
  final case class Value[A](value: A) extends Init[A]
  final case class Map[A, B](init: Init[A], f: A => B) extends Init[B]
}

final case class Setting[A](scopedKey: ScopedKey[A], init: Init[A])

final case class SettingKey[A](scope: Scope, attrKey: AttrKey[A]) extends Init[A] with ScopedKey[A] {
  def in(scope: Scope): SettingKey[A] = SettingKey(scope, attrKey)
  def <<=(init: Init[A]): Setting[A]  = Setting(this, init)
  def :=(value: A): Setting[A]        = this <<= Init.Value(value)
}

object SettingKey {
  def apply[A](s: String): SettingKey[A] = SettingKey(This, AttrKey[A](s))
}

object Main {
  def main(args: Array[String]): Unit = {
    val baseDir = SettingKey[String]("baseDir")
    val  srcDir = SettingKey[String]( "srcDir")

    val settings = Seq(
       srcDir in Global    <<= baseDir.map(_ + "/src"),
      baseDir in ThisBuild  := "/",
    )

    def check[A](s: ScopedKey[A], expected: A) = {
      val actual = null.asInstanceOf[A]
      if (actual != expected) println(s"Expected $expected, Actual $actual")
    }

    check(srcDir in ThisBuild, "/src")
  }
}
