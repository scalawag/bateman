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

package org.scalawag.bateman.json.generic

import org.scalawag.bateman.json.ProgrammerError
import org.scalawag.bateman.json.decoding.ErrorFormatters

object DetectDiscriminatorCollisions extends ErrorFormatters {
  // This function is executed purely for the side effect of throwing a ProgrammerError, if appropriate.
  def apply(label: String, discriminators: Map[String, List[String]]): Unit = {
    val dups = discriminators.filter(_._2.size > 1)
    if (dups.nonEmpty) {
      val discs = dups.map {
        case (d, cc) => s" - $label for classes ${formatAndList(cc.iterator)} use the discriminator '$d'"
      }
      val msg = discs.mkString(s"There are multiple concrete $label with the same discriminator value:\n", "\n", "")
      throw ProgrammerError(msg)
    }
  }
}
