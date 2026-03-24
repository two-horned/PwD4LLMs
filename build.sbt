val scala3Version = "3.7.4"
lazy val root = project
  .in(file("."))
  .addSbtPlugin("pl.project13.scala" % "sbt-jmh" % "0.4.8")
  .enablePlugins(JmhPlugin)
  .settings(
    Compile / run / fork := true, // necessary for custom GC options
    name := "Parsing with Derivatives for improving LLM code generation",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    libraryDependencies := Seq(
      "org.scalameta" %% "munit" % "1.0.0" % Test,
      "com.typesafe.scala-logging" %% "scala-logging" % "3.9.6",
      "first-class-derivatives" %% "first-class-derivatives" % "2.3.0",
      "org.typelevel" %% "log4cats-slf4j" % "2.8.0",
      "org.slf4j" % "slf4j-api" % "2.0.12", // logging api
      "org.slf4j" % "slf4j-simple" % "2.0.12" // simple logger
    ),
    scalacOptions ++= Seq(
      "-deprecation",
      "-feature",
      "-unchecked",
      "-Xfatal-warnings", // fail on warnings (optional)
      "-explain", // detailed explanations
      "-explain-types" // deeper type errors
    ),
    // Optimize garbage collector
    javaOptions ++= Seq(
      "-Xms1G",
      "-Xmx4G",
      "-XX:+UseG1GC",
    ),
    // Record for profiling
    javaOptions ++= Seq(
      "-XX:+FlightRecorder",
      "-XX:StartFlightRecording=filename=recording.jfr,settings=profile,dumponexit=true",
      "-XX:FlightRecorderOptions=stackdepth=256"
    )
  )
