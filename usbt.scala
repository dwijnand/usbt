package usbt

import scala.collection.immutable.ListMap
import scala.collection.mutable.{ Builder, LinkedHashMap }

final case class Name[A](value: String) { override def toString = value }

sealed abstract class Scope {
  /** Returns this scope, if it's already resolved, or the given resolved fallback. */
  def or(fallback: ResolvedScope): ResolvedScope = this match {
    case This            => fallback
    case Global          => Global
    case ThisBuild       => ThisBuild
    case x: LocalProject => x
  }
}

/** "Resolved" means it's fully-qualified and doesn't rely on any remaining context. */
sealed trait ResolvedScope extends Scope
case object This      extends Scope
case object Global    extends ResolvedScope
case object ThisBuild extends ResolvedScope // not really resolved
final case class LocalProject(id: String) extends ResolvedScope { override def toString = id }

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
  final def zipWith[B, C](x: Init[B])(f: (A, B) => C): Init[C] = Init.ZipWith[T2K[A, B]#l, C]((this, x), f.tupled, AList.tuple2)
  final def flatMap[B](f: A => Init[B]): Init[B]               = Init.Bind(this, f)

  final def zip[B](x: Init[B]): Init[(A, B)] = zipWith(x)((_, _))

  final override def toString = this match {
    case Init.Value(x)             => if (x.isInstanceOf[String]) s""""$x"""" else s"$x"
    case Init.Mapped(init, _)      => s"$init.map(<f>)"
    case Init.ZipWith(inits, _, _) => s"Init.zipWith($inits)(<f>)"
    case Init.Bind(init, _)        => s"$init.flatMap(<f>)"
    case Key(name, scope)          => if (scope == This) s"$name" else s"$scope / $name"
  }
}
object Init {
  final case class Value[A](value: A)                                                  extends Init[A]
  final case class Mapped[A, B](init: Init[A], f: A => B)                              extends Init[B]
  final case class ZipWith[K[M[x]], A](inits: K[Init], f: K[Id] => A, alist: AList[K]) extends Init[A]
  final case class Bind[A, B](init: Init[A], f: A => Init[B])                          extends Init[B]
}

final case class Setting[A](key: Key[A], init: Init[A]) {
  override def toString = key + (if (init.isInstanceOf[Init.Value[_]]) "  := " else " <<= ") + init
}

/** A map of Name -> Scope -> Init.
 *
 *  An example:
 *  baseDir -> Global -> Value(/)
 *  baseDir ->  foo   -> Value(/foo)
 */
final case class SettingMap private (underlying: ListMap[AnyName, ScopeInitMap]) {
  def getValue[A](key: Key[A]): Option[A] = evalInit(key, key.scope.or(Global))

  def getInit[A](key: Key[A], scope: ResolvedScope): Option[Init[A]] = {
    underlying
        .getOrElse(key.name, ScopeInitMap(ListMap.empty))
        .get(scope)
        .asInstanceOf[Option[Init[A]]] // guaranteed by SettingMap's builder's put signature
  }

  private def evalInit[A](init: Init[A], scope: ResolvedScope): Option[A] = {
    val eval = new ~>[Init, Option] { eval =>
      def apply[T](x: Init[T]): Option[T] = x match {
        case Init.Value(x)                 => Some(x)
        case Init.Mapped(init, f)          => eval(init).map(f)
        case Init.ZipWith(inits, f, alist) => alist.traverse[Init, Option, Id](inits, eval).map(f)
        case Init.Bind(init, f)            => eval(init).map(f).flatMap(eval(_))
        case key: Key[T]                   => getInit(key, key.scope.or(scope)).flatMap(eval(_))
      }
    }
    eval[A](init)
  }

  override def toString = underlying.mkString("SettingMap [\n  ", "\n  ", "\n]")
}

final case class ScopeInitMap(underlying: ListMap[ResolvedScope, AnyInit]) {
  def get(scope: ResolvedScope): Option[AnyInit] = {
    def getGlobal    = underlying.get(Global)
    def getThisBuild = underlying.get(ThisBuild).orElse(getGlobal)
    scope match {
      case Global          => getGlobal
      case ThisBuild       => getThisBuild
      case LocalProject(_) => underlying.get(scope).orElse(getThisBuild)
    }
  }

  override def toString =
    if (underlying.size <= 1) underlying.mkString else underlying.mkString("[ ", ", ", " ]")
}

object SettingMap {
  def fromVarargs(ss: AnySetting*): SettingMap = ss.foldLeft(newBuilder)(_.put(_)).result
  def newBuilder: Builder0 = new Builder0(LinkedHashMap.empty)

  type MapBuilder[K, V, M[_, _]] = Builder[(K, V), M[K, V]]

  final class Builder0(b: LinkedHashMap[AnyName, MapBuilder[ResolvedScope, AnyInit, ListMap]]) {
    def put[A](s: Setting[A]): Builder0 = {
      b.getOrElseUpdate(s.key.name, ListMap.newBuilder) += s.key.scope.or(Global) -> s.init
      this
    }
    def result: SettingMap = {
      val settingsMap = b.iterator.foldLeft(ListMap.newBuilder[AnyName, ScopeInitMap]) {
        case (acc, (name, b)) => acc += name -> ScopeInitMap(b.result)
      }.result
      new SettingMap(settingsMap)
    }
  }
}

object `package` {
  type Id[X] = X

  type AnyName    = Name[_]
  type AnyInit    = Init[_]
  type AnySetting = Setting[_]
}

trait Applicative[F[_]] {
  def map[S, T](f: S => T, v: F[S]): F[T]
  def pure[S](s: => S): F[S]
  def apply[S, T](f: F[S => T], v: F[S]): F[T]
}

object Applicative {
  implicit val appOption: Applicative[Option] = new Applicative[Option] {
    def map[S, T](f: S => T, v: Option[S])           = v.map(f)
    def pure[S](s: => S)                             = Some(s)
    def apply[S, T](f: Option[S => T], v: Option[S]) = f.flatMap(v.map)
  }
}

/** Natural transformation. */
trait ~>[-A[_], +B[_]] {
  def apply[T](a: A[T]): B[T]
}

/** A "higher-kinded" list containing elements of the same higher-kinded type `M[_]`. */
trait AList[K[M[x]]] {
  def traverse[M[_], N[_]: Applicative, P[_]](value: K[M], f: M ~> (N Comp P)#l): N[K[P]]
}

object AList {
  type T2List[A, B] = AList[T2K[A, B]#l]

  def tuple2[A, B]: T2List[A, B] = new AList[T2K[A, B]#l] {
    type T2[M[_]] = (M[A], M[B])

    def traverse[M[_], N[_], P[_]](t: T2[M], f: M ~> (N Comp P)#l)(implicit np: Applicative[N]): N[T2[P]] = {
      val g = (Tuple2.apply[P[A], P[B]] _).curried
      np.apply(np.map(g, f(t._1)), f(t._2))
    }
  }
}

sealed abstract class Comp[A[_], B[_]] {
  type l[T] = A[B[T]]
}

/** A utility to define the type lambda for "higher-kinded" Tuple2: `L[_] =>> (L[A], L[B])`. */
sealed abstract class T2K[A, B] {
  type l[L[x]] = (L[A], L[B])
}
