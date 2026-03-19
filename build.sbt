val scala3Version = "3.7.4"
lazy val root = project
  .in(file("."))
  .settings(
    name := "Parsing with Derivatives for improving LLM code generation",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    libraryDependencies := Seq(
      "org.scalameta" %% "munit" % "1.0.0" % Test,
      "com.typesafe.scala-logging" %% "scala-logging" % "3.9.6",
      "first-class-derivatives" %% "first-class-derivatives" % "2.0.0",
      "org.typelevel" %% "log4cats-slf4j"   % "2.8.0",
      "org.slf4j" % "slf4j-api" % "2.0.12",
      "org.slf4j" % "slf4j-simple" % "2.0.12"
    ),
    scalacOptions ++= Seq(
      "-deprecation",
      "-feature",
      "-unchecked",
      "-Xfatal-warnings", // fail on warnings (optional)
      "-explain", // detailed explanations
      "-explain-types" // deeper type errors
    )
  )
