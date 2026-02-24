// bateman -- Copyright 2021-2026 -- Justin Patterson
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
import CoverageAxis.ProjectMatrixOps

val projectBaseName = "bateman"

ThisBuild / versionScheme := Some("early-semver")
ThisBuild / sonatypeCredentialHost := "central.sonatype.com"

Global / concurrentRestrictions := Tags.limitAll(2) :: Nil

val Versions = new Object {
  val cats = "2.13.0"
  val circe = "0.14.14"
  val enumeratum = "1.9.1"
  val scalatest = "3.2.19"
  val scalamock = "7.4.0"
  val shapeless = "2.3.13"
  val shapeless3 = "3.5.0"
  val scalacheck = "1.18.1"
  val scala212 = "2.12.19"
  val scala213 = "2.13.17"
  val scala3 = "3.3.4"
}

val jvmScalaVersions = Seq(Versions.scala212, Versions.scala213, Versions.scala3)
val jsScalaVersions = Seq(Versions.scala213, Versions.scala3)

val commonSettings = Seq(
  organization := "org.scalawag.bateman",
//  scalacOptions += "-Xlog-implicits",
  Compile / unmanagedSourceDirectories ++= {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, _)) => Seq(
        (Compile / sourceDirectory).value / "scala-2"
      )
      case Some((3, _)) => Seq(
        (Compile / sourceDirectory).value / "scala-3"
      )
      case _ => Nil
    }
  },
  libraryDependencies ++= {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, n)) =>
        Seq(
          compilerPlugin("org.typelevel" % "kind-projector" % "0.13.4" cross CrossVersion.full),
          compilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1")
        )
      case Some((3, _)) => Nil // Scala 3 has built-in equivalents
      case _ => Nil
    }
  },
  scalacOptions ++= {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, n)) if n >= 13 => Seq(
        "-language:higherKinds",
        "-language:implicitConversions",
        "-Wconf:cat=deprecation&since>2.12:s,cat=deprecation&since<2.13:w",
        "-feature",
        "-Wunused:imports"
      )
      case Some((2, _)) => Seq(
        "-language:higherKinds",
        "-language:implicitConversions",
        "-feature",
        "-Ywarn-unused:imports"
      )
      case Some((3, _)) => Seq(
        "-language:implicitConversions",
        "-feature",
        "-Wunused:imports"
      )
      case _ => Seq("-feature")
    }
  },
//  addCompilerPlugin("io.tryp" % "splain" % "1.0.1" cross CrossVersion.patch),
  scalacOptions ++= {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, n)) if n <= 12 => List("-Ypartial-unification")
      case Some((3, _))            => List("-Ykind-projector", "-Xmax-inlines", "64")
      case _                       => Nil
    }
  },
  testOptions += Tests.Argument("-oDF"),
  libraryDependencies ++= Seq(
    "org.scalatest" %%% "scalatest" % Versions.scalatest,
    "org.scalacheck" %%% "scalacheck" % Versions.scalacheck,
    "org.scalamock" %%% "scalamock" % Versions.scalamock,
  ).map(_ % Test),
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
      "org.scala-lang.modules" %%% "scala-collection-compat" % "2.9.0",
      "io.github.cquiroz" %%% "scala-java-time" % "2.5.0",
      "org.typelevel" %% "cats-core" % Versions.cats,
    ),
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-laws" % Versions.cats,
      "org.typelevel" %%% "discipline-scalatest" % "2.2.0",
      "org.scalatestplus" %% "scalacheck-1-17" % "3.2.15.0",
    ).map(_ % Test),
    libraryDependencies ++= {
      if (virtualAxes.value.contains(VirtualAxis.js)) {
        // This is insecure (obviously), but it's used for unit testing only.
        CrossVersion.partialVersion(scalaVersion.value) match {
          case Some((3, _)) => Seq(
            ("org.scala-js" %%% "scalajs-fake-insecure-java-securerandom" % "1.0.0" % Test).cross(CrossVersion.for3Use2_13)
          )
          case _ => Seq(
            "org.scala-js" %%% "scalajs-fake-insecure-java-securerandom" % "1.0.0" % Test
          )
        }
      } else
        Seq.empty
    }
  )
  .jvmPlatform(scalaVersions = jvmScalaVersions)
  .jsPlatform(scalaVersions = jsScalaVersions, scalaJSLinkerConfig ~= { _.withBatchMode(true) })
  .addCoverageAxis(Versions.scala213)

val jsonLiteral = projectMatrix
  .dependsOn(json)
  .settings(commonSettings)
  .settings(
    name := s"$projectBaseName-json-literal",
    libraryDependencies ++= {
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, n)) if n <= 12 =>
          Seq(
            compilerPlugin("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.full),
            "org.scala-lang" % "scala-reflect" % scalaVersion.value
          )
        case Some((2, n)) =>
          Seq(
            "org.scala-lang" % "scala-reflect" % scalaVersion.value
          )
        case Some((3, _)) =>
          Nil // Scala 3 has built-in macro support
        case _ =>
          Nil
      }
    }
  )
  .jvmPlatform(scalaVersions = jvmScalaVersions)
  .jsPlatform(scalaVersions = jsScalaVersions, scalaJSLinkerConfig ~= { _.withBatchMode(true) })
  .addCoverageAxis(Versions.scala213)

val jsonGeneric = projectMatrix
  .dependsOn(json % "compile->compile;test->test", jsonLiteral % Test)
  .settings(commonSettings)
  .settings(
    name := s"$projectBaseName-json-generic",
    libraryDependencies ++= {
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, n)) if n <= 12 =>
          Seq(
            compilerPlugin("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.full),
            "com.chuusai" %%% "shapeless" % Versions.shapeless,
            "org.scala-lang" % "scala-reflect" % scalaVersion.value
          )
        case Some((2, n)) =>
          Seq(
            "com.chuusai" %%% "shapeless" % Versions.shapeless,
            "org.scala-lang" % "scala-reflect" % scalaVersion.value
          )
        case Some((3, _)) =>
          Seq(
            "org.typelevel" %%% "shapeless3-deriving" % Versions.shapeless3
          )
        case _ =>
          Nil
      }
    },
    libraryDependencies ++= {
      if (virtualAxes.value.contains(VirtualAxis.js)) {
        // This is insecure (obviously), but it's used for unit testing only.
        CrossVersion.partialVersion(scalaVersion.value) match {
          case Some((3, _)) => Seq(
            ("org.scala-js" %%% "scalajs-fake-insecure-java-securerandom" % "1.0.0" % Test).cross(CrossVersion.for3Use2_13)
          )
          case _ => Seq(
            "org.scala-js" %%% "scalajs-fake-insecure-java-securerandom" % "1.0.0" % Test
          )
        }
      } else
        Seq.empty
    },
    libraryDependencies += "com.lihaoyi" %%% "sourcecode" % "0.3.0" % Test
  )
  .jvmPlatform(scalaVersions = jvmScalaVersions)
  .jsPlatform(scalaVersions = jsScalaVersions, scalaJSLinkerConfig ~= { _.withBatchMode(true) })
  .addCoverageAxis(Versions.scala213)

val jsonapi = projectMatrix
  .dependsOn(jsonGeneric % "compile->compile;test->test")
  .dependsOn(json % "compile->compile;test->test")
  .settings(commonSettings)
  .settings(
    name := s"$projectBaseName-jsonapi",
  )
  .jvmPlatform(scalaVersions = jvmScalaVersions)
  .jsPlatform(scalaVersions = jsScalaVersions, scalaJSLinkerConfig ~= { _.withBatchMode(true) })
  .addCoverageAxis(Versions.scala213)

val jsonapiGeneric = projectMatrix
  .dependsOn(jsonapi % "compile->compile;test->test")
  .dependsOn(jsonGeneric)
  .settings(commonSettings)
  .settings(
    name := s"$projectBaseName-jsonapi-generic",
  )
  .jvmPlatform(scalaVersions = jvmScalaVersions)
  .jsPlatform(scalaVersions = jsScalaVersions, scalaJSLinkerConfig ~= { _.withBatchMode(true) })
  .addCoverageAxis(Versions.scala213)

val circe = projectMatrix
  .dependsOn(json)
  .settings(commonSettings)
  .settings(
    name := s"$projectBaseName-json-circe",
    libraryDependencies ++= Seq(
      "io.circe" %%% "circe-core" % Versions.circe
    )
  )
  .jvmPlatform(scalaVersions = jvmScalaVersions)
  .jsPlatform(scalaVersions = jsScalaVersions, scalaJSLinkerConfig ~= { _.withBatchMode(true) })
  .addCoverageAxis(Versions.scala213)

val enumeratum = projectMatrix
  .dependsOn(json)
  .settings(commonSettings)
  .settings(
    name := s"$projectBaseName-json-enumeratum",
    Compile / scalacOptions ++= {
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((3, _)) => Seq("-Yretain-trees")
        case _ => Nil
      }
    },
    libraryDependencies ++= Seq(
      "com.beachape" %%% "enumeratum" % Versions.enumeratum
    )
  )
  .jvmPlatform(scalaVersions = jvmScalaVersions)
  .jsPlatform(scalaVersions = jsScalaVersions, scalaJSLinkerConfig ~= { _.withBatchMode(true) })
  .addCoverageAxis(Versions.scala213)

val root = project
  .in(file("."))
  .aggregate(
    List(json, jsonGeneric, jsonLiteral, jsonapi, jsonapiGeneric, circe, enumeratum).flatMap(
      _.projectRefs
    ): _*
  )
  .settings(commonSettings)
  .settings(
    name := projectBaseName,
    publish / skip := true,
    mimaPreviousArtifacts := Set.empty,
  )
