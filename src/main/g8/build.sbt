val zioVersion = "1.0.12"
val zioHttpVersion = "1.0.0.0-RC17"
val zioJsonVersion = "0.2.0-M1"
$if(add_metrics.truthy)$
val zioZMXVersion = "0.0.8"
$endif$
val zioLoggingVersion = "0.5.12"
val logbackVersion = "1.2.6"
val zioConfigVersion = "1.0.9"

lazy val root = (project in file("."))
  .settings(
    inThisBuild(
      List(
        name := "$name$",
        organization := "$package$",
        version := "0.0.1",
        scalaVersion := "$dotty_version$",
      )
    ),
    // TODO remove, temporary solution to find zhttp-test
    // https://github.com/dream11/zio-http/issues/321
    resolvers += "Sonatype OSS Snapshots s01" at "https://s01.oss.sonatype.org/content/repositories/snapshots",
    name := "zio-quickstart",
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio" % zioVersion,
      "dev.zio" %% "zio-streams" % zioVersion,
      "io.d11" %% "zhttp" % zioHttpVersion,
      "io.d11" %% "zhttp-test" % "1.0.0.0-RC17+37-1c8ceea7-SNAPSHOT" % Test,
      "dev.zio" %% "zio-config" % zioConfigVersion,
      "dev.zio" %% "zio-config-typesafe" % zioConfigVersion,
      "dev.zio" %% "zio-logging" % zioLoggingVersion,
      "dev.zio" %% "zio-logging-slf4j" % zioLoggingVersion,
      "ch.qos.logback" % "logback-classic" % logbackVersion,
      $if(add_metrics.truthy)$
      "dev.zio" %% "zio-zmx" % zioZMXVersion,
      $endif$
      "dev.zio" %% "zio-json" % zioJsonVersion,
      "dev.zio" %% "zio-test" % zioVersion % Test,
      "dev.zio" %% "zio-test-sbt" % zioVersion % Test,
      "dev.zio" %% "zio-test-junit" % zioVersion % Test,
      "dev.zio" %% "zio-test-magnolia" % zioVersion % Test,
    ),
    testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework")),
  )
  .enablePlugins(JavaAppPackaging)
