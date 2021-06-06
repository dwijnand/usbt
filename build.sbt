val usbt = project in file(".")

inThisBuild(Def.settings(
     organization := "com.dwijnand",
          version := "0.1.0-SNAPSHOT",
     scalaVersion := "2.13.6",
   scalacOptions ++= List("-deprecation", "-feature", "-language:_", "-unchecked", "-Xlint"),
   scalacOptions  += "-Wunused:-imports",
  addCompilerPlugin("org.typelevel" % "kind-projector" % "0.13.0" cross CrossVersion.full),
  libraryDependencies ++= List(
    "org.typelevel" %% "cats-core"              % "2.4.2",
    "org.typelevel" %% "cats-mtl-core"          % "0.7.1",
    "org.scalatest" %% "scalatest"              % "3.2.5" % Test,
    "org.scalatest" %% "scalatest-freespec"     % "3.2.5" % Test, // "foo" - { "bar" - { "bar1" in {..}}}
    "org.typelevel" %% "cats-testkit-scalatest" % "2.1.2" % Test,
  ),
//Test / testOptions += Tests.Argument(TestFrameworks.ScalaTest, "-oF") // show full stack traces
))
