package test

import usbt._

object Main {
  implicit class StringWithSlash(private val self: String) extends AnyVal {
    def /(s: String) = if (self.endsWith("/")) self + s else self + "/" + s
  }

  val foo = Key[String]("foo")
  val bar = Key[String]("bar")
  val baz = Key[String]("baz")

  val bippy = Proj("bippy")

  def assertEquals[A](actual: A, expected: A, desc: String = "") = {
    implicit val z: Show[A] = _.toString
    if (actual == expected)
      None
    else if (desc == "")
      Some(show"Expected $expected, Actual $actual")
    else
      Some(show"For $desc: Expected $expected, Actual $actual")
  }

  def assertSettings[A](settings: Settings)(ss: Setting[_]*) = {
    println(show(settings))
    ss.flatMap(x => (x: @unchecked) match { case Setting(key, Init.Pure(value)) =>
      assertEquals(settings.getValue(key), Some(value), show(key))
    })
  }

  def testStd = Test {
    val        baseDir     = Key[String](       "baseDir")
    val         srcDir     = Key[String](        "srcDir")
    val      targetDir     = Key[String](     "targetDir")
    val    scalaSrcDir     = Key[String](   "scalaSrcDir")
    val        srcDirs     = Key[Seq[String]]("srcDirs")
    val crossTargetDir     = Key[String]("crossTargetDir")
    val scalaVersion       = Key[String]("scalaVersion")
    val scalaBinaryVersion = Key[String]("scalaBinaryVersion")

    val settings: Settings = Settings(Seq(
                  srcDir in Global    <<= baseDir.map(_ / "src"),
               targetDir in Global    <<= baseDir.map(_ / "target"),
             scalaSrcDir in Global    <<= srcDir.map(_ / "main/scala"),
                 srcDirs in Global    <<= scalaSrcDir.zipWith(scalaBinaryVersion)((dir, sbv) => Seq(dir, show"$dir-$sbv")),
          crossTargetDir in Global    <<= targetDir.zipWith(scalaBinaryVersion)((target, sbv) => target / show"scala-$sbv"),
                 baseDir in ThisBuild  := "/",
      scalaVersion       in ThisBuild  := "2.12.8",
      scalaBinaryVersion in ThisBuild  := "2.12",
                 baseDir in bippy      := "/bippy",
    ))

    assertSettings(settings)(
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

  def testDispatch = Test {
    assertSettings(Settings(Seq(
      foo in Global  := "g",
      foo in bippy   := "b",
      bar in bippy  <<= foo,
      baz in bippy  <<= foo in Global,
    )))(
      bar in bippy := "b",
      baz in bippy := "g",
    )
  }

  def testKeyDup = Test {
    val foo2 = Key[Int]("foo")
    val settings = Settings(Seq(foo := "a"))
    try {
      val result: Option[Int] = settings.getValue(foo2)
      List(show"Expected an exception, but got $result")
    } catch {
      case _: ClassCastException => Nil
    }
  }

  def tests = Tests(Seq(testStd, testDispatch, testKeyDup))
  def main(args: Array[String]): Unit = runTest(tests)
  def runTest(test: Test): Unit = test match {
    case TestCase(thunk) => thunk().foreach(s => println(s"${Console.RED}ERR${Console.RESET} $s"))
    case Tests(tests)    => tests.foreach(runTest)
  }

  sealed trait Test
  final case class TestCase(thunk: () => Seq[String]) extends Test
  final case class Tests(value: Seq[Test])            extends Test
  object Test {
    def apply(thunk: => Seq[String]): TestCase = TestCase(() => thunk)
  }
}
