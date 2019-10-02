package usbt

final case class Name[A](value: String)

sealed trait Scope
sealed trait ResolvedScope extends Scope // "Resolved" means doesn't rely on context (fully-qualified)
case object This      extends Scope
case object Global    extends ResolvedScope
case object ThisBuild extends ResolvedScope // not really resolved
final case class LocalProject(id: String) extends ResolvedScope

final case class Key[A](name: Name[A], scope: Scope) extends Init[A] {
  def in(scope: Scope): Key[A]       = Key(name, scope)
  def <<=(init: Init[A]): Setting[A] = Setting(this, init)
  def :=(value: A): Setting[A]       = this <<= Init.Value(value)
}

object Key {
  def apply[A](name: String): Key[A] = Key(Name(name), This)
}

sealed abstract class Init[+A] {
  final def map[B](f: A => B): Init[B]                         = Init.Mapped(this, f)
  final def zipWith[B, C](y: Init[B])(f: (A, B) => C): Init[C] = Init.ZipWith(this, y, f)
  final def flatMap[B](f: A => Init[B]): Init[B]               = Init.Bind(this, f)
}
object Init {
  final case class Value[A](value: A)                                       extends Init[A]
  final case class Mapped[A, B](init: Init[A], f: A => B)                   extends Init[B]
  final case class ZipWith[A, B, C](x: Init[A], y: Init[B], f: (A, B) => C) extends Init[C]
  final case class Bind[A, B](init: Init[A], f: A => Init[B])               extends Init[B]
}

final case class Setting[A](key: Key[A], init: Init[A])

final class ScopeOps(scope: Scope) {
  /** Returns this scope, if it's already resolved, or the given resolved fallback. */
  def or(fallback: ResolvedScope): ResolvedScope = scope match {
    case This            => fallback
    case Global          => Global
    case ThisBuild       => ThisBuild
    case x: LocalProject => x
  }
}

/** A map of Name -> Scope -> Init.
 *
 *  An example:
 *  baseDir -> Global -> Value(/)
 *  baseDir ->  foo   -> Value(/foo)
 */
final case class SettingMap private (underlying: Map[AnyName, ScopeInitMap]) {
  def getInit[A](key: Key[A], scope: ResolvedScope): Option[Init[A]] =
    underlying.get(key.name).flatMap(_.get(scope)).asInstanceOf[Option[Init[A]]]

  def getValue[A](key: Key[A]): Option[A] = {
    val init: Init[A] = key // the target `Init` to resolve the `key` itself
    val scope: ResolvedScope = key.scope.or(Global) // resolve the scope, using Global as the fallback
    evalInit(init, scope)
  }

  private def evalInit[A](init: Init[A], fallbackScope: ResolvedScope): Option[A] = {
    val eval = new ~>[Init, Option] { eval =>
      def apply[T](x: Init[T]): Option[T] = x match {
        case Init.Value(x)         => Some(x)
        case Init.Mapped(init, f)  => eval(init).map(f)
        case Init.ZipWith(x, y, f) => eval(x).flatMap(xx => eval(y).map(f(xx, _)))
        case Init.Bind(init, f)    => eval(init).map(f).flatMap(eval(_))
        case key: Key[T]           =>
          val scope = key.scope.or(fallbackScope) // resolve the scope, using the previous scope as the fallback
          getInit(key, scope).flatMap(eval(_))
      }
    }
    eval[A](init)
  }
}

object SettingMap {
  def fromVarargs(settings: AnySetting*): SettingMap = {
    import scala.collection.immutable.ListMap
    import scala.collection.mutable.{ Builder, LinkedHashMap }
    type ScopeMapBuilder = Builder[(ResolvedScope, AnyInit), Map[ResolvedScope, AnyInit]]
    val b = settings.foldLeft(new LinkedHashMap[AnyName, ScopeMapBuilder]) { (acc, setting) =>
      val Setting(Key(name, scope0), init) = setting
      val scopeMap = acc.getOrElseUpdate(name, ListMap.newBuilder)
      val scope = scope0.or(Global) // resolve the scope, using Global as the fallback
      scopeMap += scope -> init // override previous mapping at given `scope` (and `name`)
      acc
    }
    val settingsMap = b.iterator.foldLeft(ListMap.newBuilder[AnyName, ScopeInitMap]) {
      case (acc, (name, b)) => acc += name -> ScopeInitMap(b.result)
    }
    new SettingMap(settingsMap.result)
  }
}

final case class ScopeInitMap private (underlying: Map[ResolvedScope, AnyInit]) {
  def get(scope: ResolvedScope): Option[AnyInit] = {
    def getGlobal    = underlying.get(Global)
    def getThisBuild = underlying.get(ThisBuild).orElse(getGlobal)
    scope match {
      case Global          => getGlobal
      case ThisBuild       => getThisBuild
      case LocalProject(_) => underlying.get(scope).orElse(getThisBuild)
    }
  }
}

object `package` {
  type AnyName    = Name[_]
  type AnyInit    = Init[_]
  type AnySetting = Setting[_]

  def show[A](x: A)(implicit z: Show[A]) = z.show(x)

  implicit def scopeOps(scope: Scope): ScopeOps = new ScopeOps(scope)
}

/** Natural transformation. */
trait ~>[-A[_], +B[_]] {
  def apply[T](a: A[T]): B[T]
}
