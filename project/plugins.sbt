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

addSbtPlugin("com.github.sbt" % "sbt-pgp" % "2.2.1")
addSbtPlugin("org.xerial.sbt" % "sbt-sonatype" % "3.9.17")
addSbtPlugin("org.wartremover" % "sbt-wartremover" % "3.0.9")
addSbtPlugin("com.github.sbt" % "sbt-site-paradox" % "1.5.0-RC2")
addSbtPlugin("com.github.sbt" % "sbt-unidoc" % "0.5.0")
addSbtPlugin("org.scalameta" % "sbt-mdoc" % "2.3.7")
addSbtPlugin("org.scoverage" % "sbt-scoverage" % "2.0.7")

addSbtPlugin("org.scalawag.sbt" %% "sbt-git-series" % "0.1.0-pre.9")
addSbtPlugin("org.scalawag.sbt" %% "sbt-build-metadata" % "0.1.0-pre.2")

addSbtPlugin("com.eed3si9n" % "sbt-projectmatrix" % "0.9.0")
addSbtPlugin("com.timushev.sbt" % "sbt-updates" % "0.6.4")
addSbtPlugin("org.scalameta" % "sbt-scalafmt" % "2.5.0")
addSbtPlugin("ch.epfl.scala" % "sbt-version-policy" % "2.1.0")
addSbtPlugin("com.typesafe" % "sbt-mima-plugin" % "1.1.1")
addSbtPlugin("org.scalawag.sbt" % "sbt-git-flux" % "0.0.2")
addSbtPlugin("com.eed3si9n" % "sbt-buildinfo" % "0.11.0")
addSbtPlugin("org.scala-js" % "sbt-scalajs" % "1.13.0")

addSbtPlugin("org.scalawag.sbt" %% "sbt-build-metadata" % "0.1.0-pre.2")

addDependencyTreePlugin

ThisBuild / libraryDependencySchemes += "org.scala-lang.modules" %% "scala-xml" % VersionScheme.Always
