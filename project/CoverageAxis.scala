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

import sbt.Keys._
import sbt.VirtualAxis
import sbt.internal.ProjectMatrix
import scoverage.ScoverageKeys.coverageEnabled

case class CoverageAxis(idSuffix: String, directorySuffix: String) extends sbt.VirtualAxis.WeakAxis

object CoverageAxis {
  val On = CoverageAxis("_cov", "cov")
  val Off = CoverageAxis("", "")

  implicit class ProjectMatrixOps(me: ProjectMatrix) {
    def addCoverageAxis(scalaVersion: String): ProjectMatrix =
      me.customRow(
        scalaVersions = Seq(scalaVersion),
        axisValues = Seq(CoverageAxis.On, VirtualAxis.jvm),
        _.settings(
          coverageEnabled := true,
          publish / skip := true,
        )
      )
  }
}
