val v = new {
  val catsCore = "2.9.0"
  val catsEffect = "3.4.2"
  val fs2 = "3.4.0"
  val munitCatsEffect = "1.0.7"
  val scalaCheckEffect = "1.0.4"
}

inThisBuild(
  List(
    Global / onChangedBuildSource := ReloadOnSourceChanges,
    Global / excludeLintKeys ++= Set(scalacOptions),
    version := "1.0.0",
    scalaVersion := "3.2.0",
    scalacOptions ++= Seq(
      "-rewrite",
      "-indent",
      "-deprecation",
      "-feature"
    ),
    // main dependencies
    libraryDependencies ++= Seq(
      "co.fs2" %% "fs2-core" % v.fs2,
      "co.fs2" %% "fs2-io" % v.fs2,
      "org.typelevel" %% "cats-core" % v.catsCore,
      "org.typelevel" %% "cats-effect" % v.catsEffect,
      "org.typelevel" %% "munit-cats-effect-3" % v.munitCatsEffect,
      "org.typelevel" %% "scalacheck-effect" % v.scalaCheckEffect,
      "org.typelevel" %% "scalacheck-effect-munit" % v.scalaCheckEffect
    ),
    Test / parallelExecution := false,
    Compile / run / fork := true
  )
)
