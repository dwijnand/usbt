package test

import usbt._

object Main {
  implicit class StringWithSlash(private val self: String) extends AnyVal {
    def /(s: String) = if (self.endsWith("/")) self + s else self + "/" + s
  }

  // TODO: classpathOptions (in console) https://github.com/lampepfl/dotty/pull/6577/files
  // TODO: Compile/Test vs console scopes
  // TODO: alternative https://www.scala-sbt.org/1.x/docs/Scope-Delegation.html
  // TODO: add tasks
  // TODO: add input tasks
  def main(args: Array[String]): Unit = {
    def key[A](name: String): Key[A] = Key(Name(name), This)

    val foo = key[String]("foo")
    val bar = key[String]("bar")
    val baz = key[String]("baz")

    val bippy = Proj("bippy")

    def assertEquals[A: Show](actual: A, expected: A, desc: String = "") = {
      if (actual != expected)
        if (desc == "") println(show"Expected $expected, Actual $actual") else
          println(show"For $desc: Expected $expected, Actual $actual")
    }

    def assertSettings[A](settingsMap: SettingMap)(ss: AnySetting*) = {
      println(show(settingsMap))
      ss.foreach(x => (x: @unchecked) match { case Setting(key, Init.Pure(value)) =>
        assertEquals[Any](settingsMap.getValue(key), Some(value), show(key))
      })
    }

    def ignore(x: => Any) = ()

    {
      val        baseDir     = key[String](       "baseDir")
      val         srcDir     = key[String](        "srcDir")
      val      targetDir     = key[String](     "targetDir")
      val    scalaSrcDir     = key[String](   "scalaSrcDir")
      val        srcDirs     = key[Seq[String]]("srcDirs")
      val crossTargetDir     = key[String]("crossTargetDir")
      val scalaVersion       = key[String]("scalaVersion")
      val scalaBinaryVersion = key[String]("scalaBinaryVersion")

      val settingsMap: SettingMap = SettingMap.fromVarargs(
                    srcDir in Global    <<= baseDir.map(_ / "src"),
                 targetDir in Global    <<= baseDir.map(_ / "target"),
               scalaSrcDir in Global    <<= srcDir.map(_ / "main/scala"),
                   srcDirs in Global    <<= scalaSrcDir.zipWith(scalaBinaryVersion)((dir, sbv) => Seq(dir, show"$dir-$sbv")),
            crossTargetDir in Global    <<= targetDir.zipWith(scalaBinaryVersion)((target, sbv) => target / show"scala-$sbv"),
                   baseDir in ThisBuild  := "/",
        scalaVersion       in ThisBuild  := "2.12.8",
        scalaBinaryVersion in ThisBuild  := "2.12",
                   baseDir in bippy      := "/bippy",
      )

      assertSettings(settingsMap)(
        srcDir in ThisBuild    := "/src",
        srcDir in bippy        := "/bippy/src",

        targetDir in ThisBuild := "/target",
        targetDir in bippy     := "/bippy/target",

              scalaVersion in bippy := "2.12.8",
        scalaBinaryVersion in bippy := "2.12",

           scalaSrcDir in bippy := "/bippy/src/main/scala",
               srcDirs in bippy := Seq("/bippy/src/main/scala", "/bippy/src/main/scala-2.12"),
        crossTargetDir in bippy := "/bippy/target/scala-2.12",
      )
    }

    assertSettings(SettingMap.fromVarargs(
      foo in Global  := "g",
      foo in bippy   := "b",
      bar in bippy  <<= foo,
      baz in bippy  <<= foo in Global,
    ))(
      bar in bippy := "b",
      baz in bippy := "g",
    )
  }
}
