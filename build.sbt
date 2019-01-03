lazy val elevator = (project in file("."))
  .settings(
    name := "elevator",
    scalacOptions ++= Seq(
      "-feature",                 // Emit warning and location for usages of features that should be imported explicitly.
      "-Ypartial-unification"     // Enable partial unification in type constructor inference
    ),
    scalaVersion := "2.12.6"
  )
  .settings(libraryDependencies ++= Seq(
    "com.typesafe.akka" %% "akka-actor" % "2.5.19",
    "ch.qos.logback" % "logback-classic" % "1.2.3",
    "com.typesafe.scala-logging" %% "scala-logging" % "3.9.0",
    "org.typelevel" %% "cats-core" % "1.1.0" withSources() withJavadoc(),
    "com.github.pathikrit" %% "better-files" % "3.6.0",
    "io.monix" %% "monix" % "3.0.0-RC2",
    "org.scalactic" %% "scalactic" % "3.0.4",
    "org.scalatest" %% "scalatest" % "3.0.4" % "test"
  ))