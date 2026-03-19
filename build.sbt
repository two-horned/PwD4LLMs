val scala3Version = "3.7.4"
lazy val root = project
  .in(file("."))
  .settings(
    name := "Parsing with Derivatives for improving LLM code generation",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    libraryDependencies := Seq(
      "org.scalameta" %% "munit" % "1.0.0" % Test,
      "first-class-derivatives" %% "first-class-derivatives" % "2.0.0"
    ),
    scalacOptions ++= Seq(
      "-deprecation",
      "-feature",
      "-unchecked",
      "-Xfatal-warnings",   // fail on warnings (optional)
      "-explain",           // detailed explanations
      "-explain-types"      // deeper type errors
    )
  )
