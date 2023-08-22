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

import org.scalawag.sbt.gitflux.lib.FluxReleaseTag

val projectBaseName = "bateman"

ThisBuild / versionScheme := Some("early-semver")
ThisBuild / organization := "org.scalawag.bateman"

val Versions = new Object {
  val cats = "2.2.0"
  val circe = "0.14.5"
  val enumeratum = "1.6.1"
  val fastparse = "2.3.3"
  val scalatest = "3.2.16"
  val shapeless = "2.3.10"
  val scalacheck = "1.14.3"
  val scala212 = "2.12.18"
  val scala213 = "2.13.11"
  val scalaJavaTime = "2.5.0"
  val scalaCollectionCompat = "2.11.0"
}

val commonSettings = Seq(
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
    "org.scalatest" %%% "scalatest" % Versions.scalatest,
    "org.scalacheck" %%% "scalacheck" % Versions.scalacheck,
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
  // Make it so that sbt-git-flux can see the older releases.
  ThisBuild / gitFluxLegacyTagMapper := {
    case s if s.startsWith("release/") && FluxReleaseTag(s.replaceFirst("/", "-")).isDefined =>
      FluxReleaseTag(s.replaceFirst("/", "-")).get
  }
)

val json = projectMatrix
  .settings(commonSettings)
  .settings(
    name := s"$projectBaseName-json",
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-core" % Versions.cats,
      "org.scala-lang.modules" %%% "scala-collection-compat" % Versions.scalaCollectionCompat,
      "io.github.cquiroz" %%% "scala-java-time" % Versions.scalaJavaTime,
    ),
    libraryDependencies ++= Seq(
      "io.github.cquiroz" %%% "scala-java-time" % Versions.scalaJavaTime
    ).map(_ % Test)
  )
  .jvmPlatform(scalaVersions = Seq(Versions.scala212, Versions.scala213))
  .jsPlatform(scalaVersions = Seq(Versions.scala213))

val parser = projectMatrix
  .dependsOn(json)
  .settings(commonSettings)
  .settings(
    name := s"$projectBaseName-json-parser",
    libraryDependencies ++= Seq(
      "com.lihaoyi" %%% "fastparse" % Versions.fastparse
    ),
    gitFluxArtifactSince := {
      if (virtualAxes.value.contains(VirtualAxis.jvm))
        None
      else
        Some("0.1.13")
    }
  )
  .jvmPlatform(scalaVersions = Seq(Versions.scala212, Versions.scala213))
  .jsPlatform(scalaVersions = Seq(Versions.scala213))

val jsonGeneric = projectMatrix
  .dependsOn(json % "compile->compile;test->test")
  .settings(commonSettings)
  .settings(
    name := s"$projectBaseName-json-generic",
    libraryDependencies ++= Seq(
      "com.chuusai" %%% "shapeless" % Versions.shapeless,
      "org.scala-lang" % "scala-reflect" % scalaVersion.value
    ),
    libraryDependencies ++= {
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, n)) if n <= 12 =>
          List(compilerPlugin("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.full))
        case _ =>
          Nil
      }
    }
  )
  .jvmPlatform(scalaVersions = Seq(Versions.scala212, Versions.scala213))
  .jsPlatform(scalaVersions = Seq(Versions.scala213))

val jsonLiteral = projectMatrix
  .dependsOn(json)
  .settings(commonSettings)
  .settings(
    name := s"$projectBaseName-json-literal",
    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-reflect" % scalaVersion.value,
      "io.github.cquiroz" %%% "scala-java-time" % Versions.scalaJavaTime % Test,
    ),
    libraryDependencies ++= {
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, n)) if n <= 12 =>
          List(compilerPlugin("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.full))
        case _ =>
          Nil
      }
    }
  )
  .jvmPlatform(scalaVersions = Seq(Versions.scala212, Versions.scala213))
  .jsPlatform(scalaVersions = Seq(Versions.scala213))

val jsonapi = projectMatrix
  .dependsOn(jsonGeneric % "compile->compile;test->test")
  .settings(commonSettings)
  .settings(
    name := s"$projectBaseName-jsonapi"
  )
  .jvmPlatform(scalaVersions = Seq(Versions.scala212, Versions.scala213))
  .jsPlatform(scalaVersions = Seq(Versions.scala213))

val jsonapiGeneric = projectMatrix
  .dependsOn(jsonapi % "compile->compile;test->test")
  .dependsOn(jsonGeneric)
  .settings(commonSettings)
  .settings(
    name := s"$projectBaseName-jsonapi-generic",
    libraryDependencies ++= {
      if (virtualAxes.value.contains(VirtualAxis.js)) {
        // This is insecure, but it used for unit testing only.
        Seq("org.scala-js" %%% "scalajs-fake-insecure-java-securerandom" % "1.0.0" % Test)
      } else
        Seq.empty
    }
  )
  .jvmPlatform(scalaVersions = Seq(Versions.scala212, Versions.scala213))
  .jsPlatform(scalaVersions = Seq(Versions.scala213))

val circe = projectMatrix
  .dependsOn(json)
  .settings(commonSettings)
  .settings(
    name := s"$projectBaseName-json-circe",
    libraryDependencies ++= Seq(
      "io.circe" %%% "circe-core" % Versions.circe
    )
  )
  .jvmPlatform(scalaVersions = Seq(Versions.scala212, Versions.scala213))
  .jsPlatform(scalaVersions = Seq(Versions.scala213))

val enumeratum = projectMatrix
  .dependsOn(json)
  .settings(commonSettings)
  .settings(
    name := s"$projectBaseName-json-enumeratum",
    libraryDependencies ++= Seq(
      "com.beachape" %%% "enumeratum" % Versions.enumeratum
    )
  )
  .jvmPlatform(scalaVersions = Seq(Versions.scala212, Versions.scala213))
  .jsPlatform(scalaVersions = Seq(Versions.scala213))

val root = projectMatrix
  .in(file("."))
  .aggregate(json, parser, jsonGeneric, jsonLiteral, jsonapi, jsonapiGeneric, circe, enumeratum)
  .settings(commonSettings)
  .settings(
    name := projectBaseName,
    publish / skip := true,
    mimaPreviousArtifacts := Set.empty,
  )
