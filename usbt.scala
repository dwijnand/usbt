package usbt

import scala.collection.immutable.ListMap
import scala.collection.mutable.{ Builder, LinkedHashMap }

final case class Name[A](value: String) { override def toString = value }

sealed abstract class Scope extends Product with Serializable {
  /** Returns this scope, if it's already "resolved", or the given resolved fallback. */
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
case object ThisBuild extends ResolvedScope // "resolved", w/e
final case class LocalProject(id: String) extends ResolvedScope { override def toString = id }

final case class Key[A](name: Name[A], scope: Scope) extends Init[A] {
  def in(scope: Scope): Key[A]       = Key(name, scope)
  def <<=(init: Init[A]): Setting[A] = Setting(this, init)
  def :=(value: A): Setting[A]       = this <<= Init.Value(value)
}

object Key {
  def apply[A](name: String): Key[A] = Key(Name(name), This)
}

sealed abstract class Init[+A] extends Product with Serializable {
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
  override def toString = key + (if (init.isInstanceOf[AnyInit]) "  := " else " <<= ") + init
}

/** A map of Name -> Scope -> Init.
 *
 *  An example:
 *  baseDir -> Global -> Value(/)
 *  baseDir ->  foo   -> Value(/foo)
 */
final case class SettingMap private (underlying: ListMap[AnyName, ScopeInitMap]) {
  def getValue[A](key: Key[A]): A = evalInit(key, key.scope.or(Global))

  def getInit[A](key: Key[A], scope: ResolvedScope): Init[A] = {
    underlying
        .getOrElse(key.name, ScopeInitMap(ListMap.empty))
        .getOrElse(scope, sys.error(s"no $scope / ${key.name} in $this"))
        .asInstanceOf[Init[A]] // guaranteed by SettingMap's builder's put signature
  }

  private def evalInit[A](init: Init[A], scope: ResolvedScope): A = {
    val eval = new ~>[Init, Id] { eval =>
      def apply[T](x: Init[T]): T = x match {
        case Init.Value(x)                 => x
        case Init.Mapped(init, f)          => f(eval(init))
        case Init.ZipWith(inits, f, alist) => f(alist.transform(inits, eval))
        case Init.Bind(init, f)            => eval(f(eval(init)))
        case key: Key[T]                   => eval(getInit(key, key.scope.or(scope)))
      }
    }
    eval[A](init)
  }

  override def toString = underlying.mkString("SettingMap [\n  ", "\n  ", "\n]")
}

final case class ScopeInitMap(underlying: ListMap[ResolvedScope, AnyInit]) {
  def getOrElse(scope: ResolvedScope, default: => AnyInit): AnyInit = {
    def getGlobal    = underlying.getOrElse(Global, default)
    def getThisBuild = underlying.getOrElse(ThisBuild, getGlobal)
    scope match {
      case Global           => getGlobal
      case ThisBuild        => getThisBuild
      case LocalProject(id) => underlying.getOrElse(scope, getThisBuild)
    }
  }

  override def toString = if (underlying.size <= 1) underlying.mkString else underlying.mkString("[ ", ", ", " ]")
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

/** Natural transformation. */
trait ~>[-A[_], +B[_]] {
  def apply[T](a: A[T]): B[T]
}

/** A "higher-kinded" lists, that is containing elements of the same higher-kinded type `M[_]`. */
trait AList[K[M[x]]] {
  def transform[M[_], N[_]](value: K[M], f: M ~> N): K[N]
}

object AList {
  type T2List[A, B] = AList[T2K[A, B]#l]

  def tuple2[A, B]: T2List[A, B] = new AList[T2K[A, B]#l] {
    type T2[M[_]] = (M[A], M[B])
    def transform[M[_], N[_]](t: T2[M], f: M ~> N): T2[N] = (f(t._1), f(t._2))
  }
}

/** A utility to define the type lambda for "higher-kinded" Tuple2: `L[_] =>> (L[A], L[B])`. */
sealed abstract class T2K[A, B] {
  type l[L[x]] = (L[A], L[B])
}

