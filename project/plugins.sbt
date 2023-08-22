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

addSbtPlugin("com.github.sbt" % "sbt-pgp" % "2.2.1")
addSbtPlugin("org.xerial.sbt" % "sbt-sonatype" % "3.9.21")
addSbtPlugin("org.wartremover" % "sbt-wartremover" % "3.1.3")
addSbtPlugin("com.typesafe.sbt" % "sbt-site" % "1.4.1")
addSbtPlugin("org.scoverage" %% "sbt-scoverage" % "1.9.3")
addSbtPlugin("com.typesafe.sbt" % "sbt-git" % "1.0.2")
//addSbtPlugin("com.typesafe.sbt" % "sbt-native-packager" % "1.8.1")
addSbtPlugin("com.eed3si9n" % "sbt-projectmatrix" % "0.9.1")
addSbtPlugin("com.timushev.sbt" % "sbt-updates" % "0.6.4")
addSbtPlugin("org.scalameta" % "sbt-scalafmt" % "2.5.0")
addSbtPlugin("ch.epfl.scala" % "sbt-version-policy" % "2.1.3")
addSbtPlugin("com.typesafe" % "sbt-mima-plugin" % "1.1.3")
addSbtPlugin("org.scalawag.sbt" % "sbt-git-flux" % "0.0.2")
addSbtPlugin("com.eed3si9n" % "sbt-buildinfo" % "0.11.0")
addSbtPlugin("org.scala-js" % "sbt-scalajs" % "1.13.2")

addSbtPlugin("org.scalawag.sbt" %% "sbt-build-metadata" % "0.1.0-pre.2")

addDependencyTreePlugin
