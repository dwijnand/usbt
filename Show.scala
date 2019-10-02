package usbt

trait Show[A] {
  def show(x: A): String
}

object Show {
  def apply[A](implicit z: Show[A]): Show[A] = z

  def anyToString[A](x: A): String     = x.toString
  def wrappedString(s: String): String = show""""$s""""

  implicit def showAny[A]: Show[A]                    = anyToString(_)
  implicit def showString: Show[String]               = identity(_)
  implicit def showF1[A, B]: Show[A => B]             = _ => "<f>"
  implicit def showF2[A, B, C]: Show[(A, B) => C]     = _ => "<f>"
  implicit def showName[A]: Show[Name[A]]             = _.value
  implicit def showResolvedScope: Show[ResolvedScope] = show(_: Scope)
  implicit def showInitPure[A]: Show[Init.Pure[A]]    = show(_: Init[A])

  implicit def showOption[A: Show]: Show[Option[A]] = {
    case Some(x) => show"Some($x)"
    case None    => "None"
  }

  implicit def showScope: Show[Scope] = {
    case x @ This      => anyToString(x)
    case x @ Global    => anyToString(x)
    case x @ ThisBuild => anyToString(x)
    case Proj(id)      => id
  }

  implicit def showKey[A]: Show[Key[A]] = {
    case Key(name, This)  => show(name)
    case Key(name, scope) => show"$scope / $name"
  }

  implicit def showInit[A](implicit z: Show[A] = showAny[A]): Show[Init[A]] = {
    case Init.Pure(x: String)  => wrappedString(x)
    case Init.Pure(x)          => show(x)
    case Init.Map(init, f)     => show"$init.map($f)"
    case Init.ZipWith(x, y, f) => show"$x.zipWith($y)($f)"
    case Init.FlatMap(init, f) => show"$init.flatMap($f)"
    case x: Key[a]             => show(x)
  }

  implicit def showSetting[A: Show]: Show[Setting[A]] = {
    case Setting(key, init: Init.Pure[_]) => show"$key  := $init"
    case Setting(key, init)               => show"$key <<= $init"
  }

  implicit def showSettingMap: Show[SettingMap] = {
    case SettingMap(underlying) =>
      underlying
          .iterator
          .map { case (k, v) =>
            val v2 = v.iterator.map { case (scope, init) => show"$scope -> $init" }
            val v3 = if (v.size <= 1) v2.mkString else v2.mkString("[ ", ", ", " ]")
            show"$k -> $v3"
          }
          .mkString("SettingMap [\n  ", "\n  ", "\n]")
  }

  final class Shown(override val toString: String) extends AnyVal
  object Shown {
    implicit def mat[A](x: A)(implicit z: Show[A]): Shown = new Shown(z.show(x))
  }

  final class ShowInterpolator(private val sc: StringContext) extends AnyVal {
    def show(args: Shown*): String = sc.s(args: _*)
  }
}
