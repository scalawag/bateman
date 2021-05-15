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

ThisBuild / versionScheme := Some("semver-spec")

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
  organization := "org.scalawag.bateman",
  scalaVersion := "2.12.13",
  crossScalaVersions := Seq("2.12.13", "2.13.5"),
//  scalacOptions += "-Xlog-implicits",
  scalacOptions ++= Seq(
    "-language:higherKinds",
    "-language:implicitConversions",
    "-deprecation",
    "-feature"
  ),
  scalacOptions ++= {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, n)) if n <= 12 => List("-Ypartial-unification")
      case _                       => Nil
    }
  },
  testOptions += Tests.Argument("-oDF"),
  libraryDependencies ++= Seq(
    "org.scalatest" %% "scalatest" % Versions.scalatest,
    "org.scalacheck" %% "scalacheck" % Versions.scalacheck,
  ).map(_ % Test),
  publishMavenStyle := true,
  Test / publishArtifact := false,
  publishTo := {
    val nexus = "https://oss.sonatype.org/"
    if (version.value.trim.endsWith("SNAPSHOT"))
      Some("snapshots" at nexus + "content/repositories/snapshots")
    else
      Some("releases" at nexus + "service/local/staging/deploy/maven2")
  },
  pomIncludeRepository := { _ => false },
  homepage := Some(url("http://github.com/scalawag/bateman")),
  startYear := Some(2021),
  licenses += "Apache License, Version 2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0"),
  scmInfo := Some(ScmInfo(url("http://github.com/scalawag/bateman"), "scm:git:git://github.com/scalawag/bateman.git")),
  developers := List(
    Developer("justinp", "Justin Patterson", "justin@scalawag.org", url("https://github.com/justinp"))
  ),
  credentials += Credentials(
    "GnuPG Key ID",
    "gpg",
    "439444E02ED9335F91C538455283F6A358FB8629",
    "ignored"
  ),
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
    libraryDependencies ++= Seq(
      "com.chuusai" %% "shapeless" % Versions.shapeless,
      "org.scala-lang" % "scala-reflect" % scalaVersion.value
    ),
    libraryDependencies ++= {
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, n)) if n <= 12 =>
          List(compilerPlugin("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.full))
        case _ =>
          Nil
      }
    },
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
    publish / skip := true
  )
