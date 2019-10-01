package test

import usbt._

object Main {
  implicit class StringWithSlash(private val self: String) extends AnyVal {
    def /(s: String) = if (self.endsWith("/")) self + s else self + "/" + s
  }

  // classpathOptions (in console) https://github.com/lampepfl/dotty/pull/6577/files
  // Compile/Test vs console scopes
  // alternative https://www.scala-sbt.org/1.x/docs/Scope-Delegation.html
  // add tasks
  // add input tasks
  def main(args: Array[String]): Unit = {
    val bippy = LocalProject("bippy")

    val foo = Key[String]("foo")
    val bar = Key[String]("bar")
    val baz = Key[String]("baz")

    def assertEquals[A](actual: A, expected: A, desc: String = "") = {
      if (actual != expected)
        if (desc == "") println(s"Expected $expected, Actual $actual") else
          println(s"For $desc: Expected $expected, Actual $actual")
    }

    def assertSettings[A](settingsMap: SettingMap)(ss: AnySetting*) = {
      println(settingsMap)
      ss.foreach(x => (x: @unchecked) match { case Setting(key, Init.Value(value)) =>
        assertEquals(settingsMap.getValue(key), value, key.toString)
      })
    }

    def ignore(x: => Any) = ()

    {
      val        baseDir     = Key[String](       "baseDir")
      val         srcDir     = Key[String](        "srcDir")
      val      targetDir     = Key[String](     "targetDir")
      val    scalaSrcDir     = Key[String](   "scalaSrcDir")
      val        srcDirs     = Key[Seq[String]]("srcDirs")
      val crossTargetDir     = Key[String]("crossTargetDir")
      val scalaVersion       = Key[String]("scalaVersion")
      val scalaBinaryVersion = Key[String]("scalaBinaryVersion")

      val settingsMap: SettingMap = SettingMap.fromVarargs(
                    srcDir in Global    <<= baseDir.map(_ / "src"),
                 targetDir in Global    <<= baseDir.map(_ / "target"),
               scalaSrcDir in Global    <<= srcDir.map(_ / "main/scala"),
                   srcDirs in Global    <<= scalaSrcDir.zipWith(scalaBinaryVersion)((dir, sbv) => Seq(dir, s"$dir-$sbv")),
            crossTargetDir in Global    <<= targetDir.zipWith(scalaBinaryVersion)((target, sbv) => target / s"scala-$sbv"),
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
