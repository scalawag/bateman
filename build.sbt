// bateman -- Copyright 2021 -- Justin Patterson
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

val projectBaseName = "bateman"
val projectVersion = "0.1.0-SNAPSHOT"

val Versions = new Object {
  val cats = "2.2.0"
  val circe = "0.13.0"
  val enumeratum = "1.6.1"
  val fastparse = "2.3.1"
  val scalatest = "3.2.3"
  val shapeless = "2.3.3"
  val scalacheck = "1.14.1"
}

val commonSettings = Seq(
  version := projectVersion,
  organization := "org.scalawag.bateman",
  scalaVersion := "2.12.13",
  crossScalaVersions := Seq("2.12.13", "2.13.5"),
  scalacOptions += "-Xlog-implicits",
  scalacOptions ++= Seq(
    "-language:higherKinds",
    "-language:implicitConversions",
    "-Ypartial-unification",
    "-deprecation",
    "-feature"
  ),
  testOptions += Tests.Argument("-oDF"),
  libraryDependencies ++= Seq(
    "org.scalatest" %% "scalatest" % Versions.scalatest,
    "org.scalacheck" %% "scalacheck" % Versions.scalacheck,
  ).map(_ % Test),
  publishMavenStyle := true,
  publishArtifact in Test := false,
  publishTo := {
    val nexus = "https://oss.sonatype.org/"
    if (version.value.trim.endsWith("SNAPSHOT"))
      Some("snapshots" at nexus + "content/repositories/snapshots")
    else
      Some("releases" at nexus + "service/local/staging/deploy/maven2")
  },
  pomIncludeRepository := { _ => false },
)

val json = project
  .settings(commonSettings)
  .settings(
    name := s"$projectBaseName-json",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % Versions.cats
    )
  )

val parser = project
  .dependsOn(json)
  .settings(commonSettings)
  .settings(
    name := s"$projectBaseName-json-parser",
    libraryDependencies ++= Seq(
      "com.lihaoyi" %% "fastparse" % Versions.fastparse
    )
  )

val jsonGeneric = project
  .dependsOn(json % "compile->compile;test->test")
  .settings(commonSettings)
  .settings(
    name := s"$projectBaseName-json-generic",
    addCompilerPlugin(("org.scalamacros" % "paradise" % "2.1.1").cross(CrossVersion.full)),
    libraryDependencies ++= Seq(
      "com.chuusai" %% "shapeless" % Versions.shapeless,
      "org.scala-lang" % "scala-reflect" % scalaVersion.value
    )
  )

val jsonapi = project
  .dependsOn(jsonGeneric % "compile->compile;test->test")
  .settings(commonSettings)
  .settings(
    name := s"$projectBaseName-jsonapi",
    libraryDependencies ++= Seq(
    )
  )

val jsonapiGeneric = project
  .dependsOn(jsonapi % "compile->compile;test->test")
  .dependsOn(jsonGeneric)
  .settings(commonSettings)
  .settings(
    name := s"$projectBaseName-jsonapi-generic",
  )

val circe = project
  .dependsOn(json)
  .settings(commonSettings)
  .settings(
    name := s"$projectBaseName-json-circe",
    libraryDependencies ++= Seq(
      "io.circe" %% "circe-core" % Versions.circe
    )
  )

val enumeratum = project
  .dependsOn(json)
  .settings(commonSettings)
  .settings(
    name := s"$projectBaseName-json-enumeratum",
    libraryDependencies ++= Seq(
      "com.beachape" %% "enumeratum" % Versions.enumeratum
    )
  )


val root = (project in file("."))
  .aggregate(json, parser, jsonGeneric, jsonapi, jsonapiGeneric, circe)
  .settings(
    name := s"$projectBaseName-build",
    skip in publish := true
  )
