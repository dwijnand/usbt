val usbt = project in file(".")

organization in ThisBuild := "com.dwijnand"
     version in ThisBuild := "0.1.0-SNAPSHOT"
scalaVersion in ThisBuild := "2.12.10"

scalacOptions ++= Seq(
  "-feature",
  "-language:higherKinds",
)
