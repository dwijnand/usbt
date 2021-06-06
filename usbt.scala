package usbt

final case class Name(value: String)

final case class Key[A](name: Name, scope: Scope) extends Init[A] {
  def in(scope: Scope): Key[A]       = Key(name, scope)
  def <<=(init: Init[A]): Setting[A] = Setting(this, init)
  def :=(value: A): Setting[A]       = this <<= Init.Pure(value)
}
object Key {
  def apply[A](name: String) = new Key[A](Name(name), This)
}

sealed abstract class Init[+A] {
  final def map[B](f: A => B): Init[B]                         = Init.Map(this, f)
  final def zipWith[B, C](y: Init[B])(f: (A, B) => C): Init[C] = Init.ZipWith(this, y, f)
  final def flatMap[B](f: A => Init[B]): Init[B]               = Init.FlatMap(this, f)
}
object Init {
  final case class Pure[A](value: A)                                        extends Init[A]
  final case class Map[A, B](init: Init[A], f: A => B)                      extends Init[B]
  final case class ZipWith[A, B, C](x: Init[A], y: Init[B], f: (A, B) => C) extends Init[C]
  final case class FlatMap[A, B](init: Init[A], f: A => Init[B])            extends Init[B]
}

final case class Setting[A](key: Key[A], init: Init[A])

sealed trait Scope {
  def or(fallback: ResolvedScope): ResolvedScope = this match {
    case This      => fallback
    case Global    => Global
    case ThisBuild => ThisBuild
    case x: Proj   => x
  }
}
sealed trait ResolvedScope        extends Scope // "Resolved" means doesn't rely on context (fully-qualified)
case object This                  extends Scope
case object Global                extends ResolvedScope
case object ThisBuild             extends ResolvedScope
final case class Proj(id: String) extends ResolvedScope

object Scope {
  val delegates: ResolvedScope => LazyList[ResolvedScope] = {
    case scope @ (_: Proj) => scope #:: delegates(ThisBuild)
    case scope @ ThisBuild => scope #:: delegates(Global)
    case scope @ Global    => scope #:: LazyList.empty
  }
}

final case class Settings(value: Seq[Setting[_]]) {
  def getValue[A](key: Key[A]): Option[A] = evalInit(key.scope.or(Global))(key)

  private def evalInit(fallbackScope: ResolvedScope): Init ~> Option = new ~>[Init, Option] { eval =>
    def apply[T](x: Init[T]): Option[T] = x match {
      case Init.Pure(x)          => Some(x)
      case Init.Map(init, f)     => eval(init).map(f)
      case Init.ZipWith(x, y, f) => eval(x).flatMap(a => eval(y).map(f(a, _)))
      case Init.FlatMap(init, f) => eval(init).flatMap(s => eval(f(s)))
      case key: Key[T]           => getInit(key, key.scope.or(fallbackScope)).flatMap(eval(_))
    }
  }

  private def getInit[A](key: Key[A], scope: ResolvedScope): Option[Init[A]] = {
    lookup.get(key.name).flatMap { scopeToInitMap =>
      Scope.delegates(scope).flatMap(scopeToInitMap.get(_)).headOption
    }.asInstanceOf[Option[Init[A]]]
  }

  private val lookup: Map[Name, Map[ResolvedScope, Init[_]]] = {
    value.groupMapReduce(_.key.name)(s => Map(s.key.scope.or(Global) -> s.init))(_ ++ _)
  }
}
