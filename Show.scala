package usbt

trait Show[A] {
  def show(x: A): String
}

object Show {
  def apply[A](implicit z: Show[A]): Show[A] = z

  implicit def showString: Show[String]               = identity(_)
  implicit def showF1[A, B]: Show[A => B]             = _ => "<f>"
  implicit def showF2[A, B, C]: Show[(A, B) => C]     = _ => "<f>"
  implicit def showName[A]: Show[Name[A]]             = _.value
  implicit def showResolvedScope: Show[ResolvedScope] = show(_: Scope)
  implicit def showInitValue[A]: Show[Init.Value[A]]  = show(_: Init[A])

  val showWrappedString: Show[String] = x => s""""${show(x)}""""

  implicit def showScope: Show[Scope] = {
    case This             => "This"
    case Global           => "Global"
    case ThisBuild        => "ThisBuild"
    case LocalProject(id) => id
  }

  implicit def showKey[A]: Show[Key[A]] = {
    case Key(name, This)  => show(name)
    case Key(name, scope) => s"${show(scope)} / ${show(name)}"
  }

  implicit def showInit[A](implicit z: Show[A] = null): Show[Init[A]] = {
    val oldZ = z;
    {
      implicit val z: Show[A] = if (oldZ == null) _.toString else oldZ;
      {
        case Init.Value(x: String) => showWrappedString.show(x)
        case Init.Value(x)         => show(x)
        case Init.Mapped(init, f)  => s"${show(init)}.map(${show(f)})"
        case Init.ZipWith(x, y, f) => s"${show(x)}.zipWith(${show(y)}(${show(f)})"
        case Init.Bind(init, f)    => s"${show(init)}.flatMap(${show(f)})"
        case x: Key[a]             => show(x)
      }
    }
  }

  implicit def showSetting[A: Show]: Show[Setting[A]] = {
    case Setting(key, init: Init.Value[_]) => s"${show(key)}  := ${show(init)}"
    case Setting(key, init)                => s"${show(key)} <<= ${show(init)}"
  }

  implicit def showSettingMap: Show[SettingMap] = {
    case SettingMap(underlying) =>
      underlying.iterator
          .map { case (k, v) => s"${show(k)} -> ${show(v)}" }
          .mkString("SettingMap [\n  ", "\n  ", "\n]")
  }

  implicit def showScopeInitMap: Show[ScopeInitMap] = {
    case ScopeInitMap(underlying0) =>
      val underlying = underlying0.iterator.map { case (scope, init) => s"${show(scope)} -> ${show(init)}" }
      if (underlying0.size <= 1) underlying.mkString
      else underlying.mkString("[ ", ", ", " ]")
  }
}
