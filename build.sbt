
name := "Scala"

version := "0.1"

scalaVersion := "2.11.12"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4"

scalacOptions ++= Seq(
  "-deprecation",
  "-unchecked",
  "-optimise"//,
//  "-Yinline-warnings"
)


fork := true

javaOptions += "-Xmx3G"

parallelExecution in Test := false

libraryDependencies ++= Seq(
  "com.storm-enroute" %% "scalameter-core" % "0.6",
  "org.scala-lang.modules" %% "scala-swing" % "1.0.1",
  "com.github.scala-blitz" %% "scala-blitz" % "1.1",
  "org.scalactic" %% "scalactic" % "2.2.6",
  "org.scalatest" %% "scalatest" % "2.2.6" % "test",

  "org.apache.spark" %% "spark-core" % "2.1.0",
  "org.apache.spark" %% "spark-sql" % "2.1.0"
)
/**
  * Force sbt to use scala 2.11.5,
  * otherwise, some dependence will upgrade scala version to 2.11.7
  * in which `sort1` does not exist
  */
dependencyOverrides += "org.scala-lang" % "scala-library" % scalaVersion.value