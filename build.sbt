val usbt = project in file(".")

organization in ThisBuild := "com.dwijnand"
     version in ThisBuild := "0.1.0-SNAPSHOT"
scalaVersion in ThisBuild := "2.13.1"

scalacOptions ++= Seq(
  "-deprecation",
  "-feature",
  "-language:existentials,higherKinds,implicitConversions",
)
