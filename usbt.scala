package usbt

final case class Name[A](value: String)

sealed abstract class Init[+A]
object Init {
  final case class Pure[A](value: A)                                        extends Init[A]
  final case class Map[A, B](init: Init[A], f: A => B)                      extends Init[B]
  final case class ZipWith[A, B, C](x: Init[A], y: Init[B], f: (A, B) => C) extends Init[C]
  final case class FlatMap[A, B](init: Init[A], f: A => Init[B])            extends Init[B]

  implicit class InitOps[A](private val init: Init[A]) extends AnyVal {
    final def map[B](f: A => B): Init[B]                         = Init.Map(init, f)
    final def zipWith[B, C](y: Init[B])(f: (A, B) => C): Init[C] = Init.ZipWith(init, y, f)
    final def flatMap[B](f: A => Init[B]): Init[B]               = Init.FlatMap(init, f)
  }
}

final case class Key[A](name: Name[A], scope: Scope) extends Init[A] {
  def in(scope: Scope): Key[A]       = Key(name, scope)
  def <<=(init: Init[A]): Setting[A] = Setting(this, init)
  def :=(value: A): Setting[A]       = this <<= Init.Pure(value)
}

final case class Setting[A](key: Key[A], init: Init[A])

sealed trait Scope
sealed trait ResolvedScope        extends Scope // "Resolved" means doesn't rely on context (fully-qualified)
case object This                  extends Scope
case object Global                extends ResolvedScope
case object ThisBuild             extends ResolvedScope
final case class Proj(id: String) extends ResolvedScope

object Scope {
  implicit class ScopeOps(private val scope: Scope) extends AnyVal {
    /** Returns this scope, if it's already resolved, or the given resolved fallback. */
    def or(fallback: ResolvedScope): ResolvedScope = scope match {
      case This      => fallback
      case Global    => Global
      case ThisBuild => ThisBuild
      case x: Proj   => x
    }
  }

  val delegates: ResolvedScope => Stream[ResolvedScope] = {
    case scope @ (_: Proj) => scope #:: delegates(ThisBuild)
    case scope @ ThisBuild => scope #:: delegates(Global)
    case scope @ Global    => scope #:: Stream.empty
  }
}

/** A map of Name -> Scope -> Init.
 *
 *  An example:
 *  baseDir -> Global -> Init.Pure(/)
 *  baseDir -> bippy  -> Init.Pure(/foo)
 */
final case class SettingMap private (underlying: Map[AnyName, Map[ResolvedScope, AnyInit]]) {
  def getInit[A](key: Key[A], scope: ResolvedScope): Option[Init[A]] = {
    underlying.get(key.name).flatMap { scopeToInitMap =>
      Scope.delegates(scope).flatMap(scopeToInitMap.get(_)).headOption
    }.asInstanceOf[Option[Init[A]]] // TODO: test this failure
  }

  def getValue[A](key: Key[A]): Option[A] = evalInit(key.scope.or(Global))(key)

  private def evalInit[A](fallbackScope: ResolvedScope): Init[A] => Option[A] = {
    val eval = new ~>[Init, Option] { eval =>
      def apply[T](x: Init[T]): Option[T] = x match {
        case Init.Pure(x)          => Some(x)
        case Init.Map(init, f)     => eval(init).map(f)
        case Init.ZipWith(x, y, f) => eval(x).flatMap(a => eval(y).map(f(a, _)))
        case Init.FlatMap(init, f) => eval(init).flatMap(s => eval(f(s)))
        case key: Key[T]           => getInit(key, key.scope.or(fallbackScope)).flatMap(eval(_))
      }
    }
    eval[A]
  }
}

object SettingMap {
  import scala.collection.immutable.ListMap
  import scala.collection.mutable

  def fromVarargs(settings: AnySetting*): SettingMap = {
    val b = settings.foldLeft(mutable.LinkedHashMap.empty[AnyName, mutable.LinkedHashMap[ResolvedScope, AnyInit]]) {
      case (acc, Setting(Key(name, scope0), init)) =>
        val scopeMap = acc.getOrElse(name, mutable.LinkedHashMap.empty[ResolvedScope, AnyInit])
        val scope = scope0.or(Global) // resolve the scope, replacing This with Global
        scopeMap += scope -> init // override previous mapping at given `scope` (and `name`)
        acc += name -> scopeMap
    }
    new SettingMap(toListMap(b)(toListMap(_)(identity)))
  }

  private def toListMap[K, V, V2](xs: Iterable[(K, V)])(f: V => V2): Map[K, V2] = {
    xs.foldLeft(ListMap.newBuilder[K, V2]) { case (acc, (k, v)) => acc += k -> f(v) }.result
  }
}

object `package` {
  type AnyName    = Name[_]
  type AnyInit    = Init[_]
  type AnySetting = Setting[_]

  def show[A](x: A)(implicit z: Show[A]) = z.show(x)
  implicit def showInterpolator(sc: StringContext): Show.ShowInterpolator = new Show.ShowInterpolator(sc)
}

/** Natural transformation. */
trait ~>[-A[_], +B[_]] {
  def apply[T](a: A[T]): B[T]
}
