// bateman -- Copyright 2021-2023 -- Justin Patterson
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

Global / concurrentRestrictions := Tags.limitAll(2) :: Nil

val Versions = new Object {
  val cats = "2.9.0"
  val circe = "0.14.4"
  val enumeratum = "1.7.2"
  val scalatest = "3.2.15"
  val scalamock = "5.2.0"
  val shapeless = "2.3.10"
  val scalacheck = "1.17.0"
  val scala212 = "2.12.17"
  val scala213 = "2.13.10"
  val scala3 = "3.2.1"
}

val jvmScalaVersions = Seq(Versions.scala212, Versions.scala213 /*, Versions.scala3*/ )
val jsScalaVersions = jvmScalaVersions

val commonSettings = Seq(
  organization := "org.scalawag.bateman",
//  scalacOptions += "-Xlog-implicits",
  libraryDependencies ++= {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, n)) =>
        Seq(compilerPlugin("org.typelevel" % "kind-projector" % "0.13.2" cross CrossVersion.full))
      case _ => Nil
    }
  },
  scalacOptions ++= Seq(
    "-language:higherKinds",
    "-language:implicitConversions",
    // Only report deprecations after 2.12 (our earliest supported version)
    "-Wconf:cat=deprecation&since>2.12:s,cat=deprecation&since<2.13:w",
    "-feature"
  ),
  scalacOptions ++= {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, n)) if n <= 12 => List("-Ypartial-unification")
      case Some((3, _))            => List("-Ykind-projector")
      case _                       => Nil
    }
  },
  testOptions += Tests.Argument("-oDF"),
  libraryDependencies ++= Seq(
    "org.scalatest" %%% "scalatest" % Versions.scalatest,
    "org.scalacheck" %%% "scalacheck" % Versions.scalacheck,
    "org.scalamock" %%% "scalamock" % Versions.scalamock,
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
        Seq("org.scala-js" %%% "scalajs-fake-insecure-java-securerandom" % "1.0.0" % Test)
      } else
        Seq.empty
    }
  )
  .jvmPlatform(scalaVersions = jvmScalaVersions)
  .jsPlatform(scalaVersions = jsScalaVersions)
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
        case _ =>
          Nil
      }
    }
  )
  .jvmPlatform(scalaVersions = jvmScalaVersions)
  .jsPlatform(scalaVersions = jsScalaVersions)
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
        case _ =>
          Nil
      }
    },
    libraryDependencies ++= {
      if (virtualAxes.value.contains(VirtualAxis.js)) {
        // This is insecure (obviously), but it's used for unit testing only.
        Seq("org.scala-js" %%% "scalajs-fake-insecure-java-securerandom" % "1.0.0" % Test)
      } else
        Seq.empty
    },
    libraryDependencies += "com.lihaoyi" %%% "sourcecode" % "0.3.0" % Test
  )
  .jvmPlatform(scalaVersions = jvmScalaVersions)
  .jsPlatform(scalaVersions = jsScalaVersions)
  .addCoverageAxis(Versions.scala213)

val jsonapi = projectMatrix
  .dependsOn(jsonGeneric % "compile->compile;test->test")
  .dependsOn(json % "compile->compile;test->test")
  .settings(commonSettings)
  .settings(
    name := s"$projectBaseName-jsonapi",
  )
  .jvmPlatform(scalaVersions = jvmScalaVersions)
  .jsPlatform(scalaVersions = jsScalaVersions)
  .addCoverageAxis(Versions.scala213)

val jsonapiGeneric = projectMatrix
  .dependsOn(jsonapi % "compile->compile;test->test")
  .dependsOn(jsonGeneric)
  .settings(commonSettings)
  .settings(
    name := s"$projectBaseName-jsonapi-generic",
  )
  .jvmPlatform(scalaVersions = jvmScalaVersions)
  .jsPlatform(scalaVersions = jsScalaVersions)
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
  .jsPlatform(scalaVersions = jsScalaVersions)
  .addCoverageAxis(Versions.scala213)

val enumeratum = projectMatrix
  .dependsOn(json)
  .settings(commonSettings)
  .settings(
    name := s"$projectBaseName-json-enumeratum",
    libraryDependencies ++= Seq(
      "com.beachape" %%% "enumeratum" % Versions.enumeratum
    )
  )
  .jvmPlatform(scalaVersions = jvmScalaVersions)
  .jsPlatform(scalaVersions = jsScalaVersions)
  .addCoverageAxis(Versions.scala213)

val docs = project
  .in(file("docs"))
  .settings(commonSettings)
  // This is needed to build the doc examples with mdoc
  .dependsOn(
    jsonLiteral.jvm(Versions.scala212),
    enumeratum.jvm(Versions.scala212),
    circe.jvm(Versions.scala212),
    jsonapiGeneric.jvm(Versions.scala212)
  )
  .enablePlugins(ParadoxSitePlugin, ScalaUnidocPlugin)
  .settings(
    paradoxTheme := Some(builtinParadoxTheme("generic")),
    crossPaths := false,
    autoAPIMappings := true,
    paradoxNavigationDepth := 3,
    Compile / paradoxProperties ++= Map(
      "scaladoc.base_url" -> ".../api",
      "scaladoc.cats.base_url" -> "https://typelevel.org/cats/api/",
    ),
    Global / excludeLintKeys += paradoxNavigationDepth,
    // unidoc seems to barf  on Scala 2.13 projects with a binary incompatibility error.
    ScalaUnidoc / unidoc / unidocProjectFilter := inProjects(
      json.jvm(Versions.scala212),
      jsonGeneric.jvm(Versions.scala212),
      jsonLiteral.jvm(Versions.scala212),
      jsonapi.jvm(Versions.scala212),
      jsonapiGeneric.jvm(Versions.scala212),
      circe.jvm(Versions.scala212),
      enumeratum.jvm(Versions.scala212),
    ),
    ScalaUnidoc / siteSubdirName := "api",
    addMappingsToSiteDir(ScalaUnidoc / packageDoc / mappings, ScalaUnidoc / siteSubdirName),
  )
  .enablePlugins(MdocPlugin)
  .settings(
    // Use mdoc and a preprocessor for paradox (to evaluate code fences only)
    mdocIn := baseDirectory.value / "src" / "main" / "mdoc",
    mdocOut := (Compile / paradox / sourceManaged).value,
    // Without this, mdoc will puke on custom paradox link types
    mdocExtraArguments ++= Seq("--no-link-hygiene"),
    // For some reason, paradox isn't looking in its source_managed directory, so explicitly set this.
    Compile / paradox / sourceDirectory := mdocOut.value,
    // Always run mdoc before paradox
    Compile / paradox := (Compile / paradox).dependsOn(mdoc.toTask("")).value,
  )
  .settings(
    libraryDependencies += "org.scalawag.sarong" %% "sarong" % "0.1.1"
  )

val root = project
  .in(file("."))
  .aggregate(docs)
  .aggregate(
    List(json, jsonGeneric, jsonLiteral, jsonapi, jsonapiGeneric, circe, enumeratum).flatMap(
      _.projectRefs
    ): _*
  )
  .settings(commonSettings)
  .settings(
    name := projectBaseName,
    publish / skip := true,
    mimaPreviousArtifacts := Set.empty
  )
