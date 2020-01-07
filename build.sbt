import sbt._

lazy val fp = (project in file(".")).
  settings (
    name := "custom-io-for-env",
    organization := "com.example",
    version := "0.1-SNAPSHOT",
    scalaVersion := "2.13.1"
    // add other settings here
  )

/* scala versions and options */
scalaVersion := "2.13.1"

addCompilerPlugin("org.typelevel" % "kind-projector_2.13.1" % "0.11.0")

// These options will be used for *all* versions.
scalacOptions ++= Seq(
  "-deprecation"
  , "-unchecked"
  , "-encoding", "UTF-8"
  , "-Xlint"
  , "-Xverify"
  , "-feature"
  , "-language:_"
)

javacOptions ++= Seq("-Xlint:unchecked", "-Xlint:deprecation", "-source", "1.7", "-target", "1.7")

val CatsVersion = "2.0.0"
val CatsEffectVersion = "2.0.0"
val ZIOVersion = "1.0.0-RC11-1"
val ZIOInteropCatsVersion = "2.0.0.0-RC2"

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % CatsVersion,
  "org.typelevel" %% "cats-effect" % CatsEffectVersion,

  "dev.zio" %% "zio" % ZIOVersion,
  "dev.zio" %% "zio-interop-cats" % ZIOInteropCatsVersion,
)

resolvers ++= Seq(
  "Typesafe Snapshots" at "http://repo.typesafe.com/typesafe/snapshots/",
  "Secured Central Repository" at "https://repo1.maven.org/maven2",
  Resolver.sonatypeRepo("snapshots")
)

