package usbt

import java.io.File

object `package` {
  def file(s: String) = new File(s)

  implicit class RichFile(private val f: File) extends AnyVal {
    def /(s: String): File = new File(f, s)
  }
}

sealed trait Reference
final case object ThisBuild extends Reference
final case class LocalProject(id: String) extends Reference

sealed trait ScopeAxis[+A]
case object This extends ScopeAxis[Nothing]
case object Zero extends ScopeAxis[Nothing]
final case class Select[A](x: A) extends ScopeAxis[A]

final case class Scope(r: ScopeAxis[Reference])
object Scope {
  val ThisScope: Scope = Scope(This)
}

final case class AttributeKey[A](name: String)

sealed trait Scoped[A] {
  def scope: Scope
  def attrKey: AttributeKey[A]
}

final case class ScopedKey[A](scope: Scope, attrKey: AttributeKey[A]) extends Scoped[A]

sealed trait Initialize[A]
final case class Value[A](value: () => A) extends Initialize[A]
final case class GetValue[A, B](scopedKey: ScopedKey[A], transform: A => B) extends Initialize[B]

final case class Setting[A](scopedKey: ScopedKey[A], init: Initialize[A])

final case class SettingKey[A](scope: Scope, attrKey: AttributeKey[A])
    extends Initialize[A] with Scoped[A]
{
  def in(r: Reference): SettingKey[A] = in(Select(r))
  def in(r: ScopeAxis[Reference]): SettingKey[A] = in(Scope(r))
  def in(scope: Scope): SettingKey[A] = SettingKey(scope, attrKey)

  def scopedKey: ScopedKey[A] = ScopedKey[A](scope, attrKey)

  def <<=(init: Initialize[A]): Setting[A] = Setting(scopedKey, init)
  def :=(value: => A): Setting[A] = this <<= Value(value _)

  def map[B](f: A => B): Initialize[B] = GetValue(scopedKey, f)
}

object SettingKey {
  def apply[A](s: String): SettingKey[A] = SettingKey[A](Scope.ThisScope, AttributeKey[A](s))
}

final case class Project(id: String, settings: Seq[Setting[_]]) {
  def settings(ss: Setting[_]*) = copy(settings = this.settings ++ ss)
}
object Project {
  def apply(id: String): Project = Project(id, Nil)

  implicit def projectToLocalProject(p: Project): LocalProject = LocalProject(p.id)
}

object Main {
  def main(args: Array[String]): Unit = {
    val baseDir = SettingKey[File]("baseDir")
    val srcDir = SettingKey[File]("srcDir")

    val r = Project("r") settings (
      baseDir in ThisBuild  := file("/"),
       srcDir in ThisBuild <<= baseDir map (_ / "src")
    )

    val a = Project("a") settings (baseDir := file("/a"))
    val b = Project("b") settings (baseDir := file("/b"))

    def check[A](s: Scoped[A], expected: String) = {
      val actual = ""
      if (actual != expected) println(s"Expected $expected, Actual $actual")
    }

    check(baseDir in ThisBuild, "/")
    check(baseDir in a, "/a")
    check(baseDir in b, "/b")

    check(srcDir in ThisBuild, "/src")
    check(srcDir in a, "/a/src")
    check(srcDir in b, "/b/src")
  }
}
