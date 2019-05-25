package usbt

sealed trait Reference
case object ThisBuild extends Reference
final case class LocalProject(id: String) extends Reference

sealed trait ScopeAxis[+A]
case object This extends ScopeAxis[Nothing]
case object Global extends ScopeAxis[Nothing]
final case class Select[A](x: A) extends ScopeAxis[A]
object ScopeAxis {
  implicit def scopeAxisToScope(axis: ScopeAxis[Nothing]): Scope = Scope(axis)
}

final case class Scope(r: ScopeAxis[Reference])

final case class AttributeKey[A](name: String)

sealed trait ScopedKey[A] {
  def scope: Scope
  def attrKey: AttributeKey[A]
}

sealed abstract class Initialize[A] {
  final def map[B](f: A => B): Initialize[B]                 = flatMap(a => Initialize.of(f(a)))
  final def flatMap[B](f: A => Initialize[B]): Initialize[B] = new Initialize.Bind(this, f)
}
object Initialize {
  final class Thunk[A](val thunk: () => A) extends Initialize[A]
  final class Bind[A, B](val in: Initialize[A], val f: A => Initialize[B]) extends Initialize[B]
  def of[A](thunk: => A): Thunk[A] = new Thunk(thunk _)
}

final case class Setting[A](scopedKey: ScopedKey[A], init: Initialize[A])

final case class SettingKey[A](scope: Scope, attrKey: AttributeKey[A]) extends Initialize[A] with ScopedKey[A] {
  def in(r: Reference): SettingKey[A]            = in(Select(r))
  def in(r: ScopeAxis[Reference]): SettingKey[A] = in(Scope(r))
  def in(scope: Scope): SettingKey[A]            = SettingKey(scope, attrKey)

  def <<=(init: Initialize[A]): Setting[A] = Setting(this, init)
  def :=(value: => A): Setting[A]          = this <<= Initialize.of(value)
}

object SettingKey {
  def apply[A](s: String): SettingKey[A] = SettingKey(This, AttributeKey[A](s))
}

final case class Project(id: String, settings: Seq[Setting[_]]) {
  def settings(ss: Setting[_]*) = copy(settings = settings ++ ss)
}
object Project {
  def apply(id: String): Project = Project(id, Nil)
  implicit def projectToLocalProject(p: Project): LocalProject = LocalProject(p.id)
}

object Main {
  def main(args: Array[String]): Unit = {
    val baseDir = SettingKey[String]("baseDir")
    val  srcDir = SettingKey[String]( "srcDir")

    val root = Project("root") settings (
      baseDir in ThisBuild  := "/",
       srcDir in ThisBuild <<= baseDir.map(_ + "/src"),
    )

    val foo = Project("foo").settings(baseDir := "/foo")

    def check[A](s: ScopedKey[A], expected: A) = {
      val actual = null.asInstanceOf[A]
      if (actual != expected) println(s"Expected $expected, Actual $actual")
    }

    check(baseDir in ThisBuild, "/")
    check(baseDir in foo, "/foo")

    check(srcDir in ThisBuild, "/src")
    check(srcDir in foo, "/foo/src")
  }
}
