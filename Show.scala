package usbt

trait Show[A] {
  def show(x: A): String
}

object Show {
  def apply[A](implicit z: Show[A]): Show[A] = z

  implicit def showA[A]: Show[A]                      = _.toString
  implicit def showAny: Show[Any]                     = showA
  implicit def showString: Show[String]               = identity(_)
  implicit def showF1[A, B]: Show[A => B]             = _ => "<f>"
  implicit def showF2[A, B, C]: Show[(A, B) => C]     = _ => "<f>"
  implicit def showName[A]: Show[Name[A]]             = _.value
  implicit def showResolvedScope: Show[ResolvedScope] = show(_: Scope)
  implicit def showInitValue[A]: Show[Init.Value[A]]  = show(_: Init[A])

  val showWrappedString: Show[String] = x => show""""$x""""

  implicit def showOption[A: Show]: Show[Option[A]] = {
    case Some(x) => show"Some($x)"
    case None    => "None"
  }

  implicit def showScope: Show[Scope] = {
    case This             => "This"
    case Global           => "Global"
    case ThisBuild        => "ThisBuild"
    case LocalProject(id) => id
  }

  implicit def showKey[A]: Show[Key[A]] = {
    case Key(name, This)  => show(name)
    case Key(name, scope) => show"$scope / $name"
  }

  implicit def showInit[A](implicit z: Show[A] = showA): Show[Init[A]] = {
    case Init.Value(x: String) => showWrappedString.show(x)
    case Init.Value(x)         => show(x)
    case Init.Mapped(init, f)  => show"$init.map($f)"
    case Init.ZipWith(x, y, f) => show"$x.zipWith($y($f)"
    case Init.FlatMap(init, f) => show"$init.flatMap($f)"
    case x: Key[a]             => show(x)
  }

  implicit def showSetting[A: Show]: Show[Setting[A]] = {
    case Setting(key, init: Init.Value[_]) => show"$key  := $init"
    case Setting(key, init)                => show"$key <<= $init"
  }

  implicit def showSettingMap: Show[SettingMap] = {
    case SettingMap(underlying) =>
      underlying.iterator
          .map { case (k, v) => show"$k -> $v" }
          .mkString("SettingMap [\n  ", "\n  ", "\n]")
  }

  implicit def showScopeInitMap: Show[ScopeInitMap] = {
    case ScopeInitMap(underlying0) =>
      val underlying = underlying0.iterator.map { case (scope, init) => show"$scope -> $init" }
      if (underlying0.size <= 1) underlying.mkString
      else underlying.mkString("[ ", ", ", " ]")
  }

  final class Shown(override val toString: String) extends AnyVal
  object Shown {
    implicit def mat[A](x: A)(implicit z: Show[A]): Shown = new Shown(z.show(x))
  }

  final class ShowInterpolator(private val sc: StringContext) extends AnyVal {
    def show(args: Shown*): String = sc.s(args: _*)
  }
}
