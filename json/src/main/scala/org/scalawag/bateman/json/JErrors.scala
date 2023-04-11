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

package org.scalawag.bateman.json

import cats.data.NonEmptyChain
import org.scalawag.bateman.json.JErrors.formatErrorReport

case class JErrors(errors: NonEmptyChain[JError]) extends Exception(formatErrorReport(errors))

object JErrors {

  /** Creates a human-readable, sorted, deduplicated list of error messages. */
  def formatErrorReport(errors: NonEmptyChain[JError]): String =
    JErrorFormatters.maybeBullets(errors.toChain.toList.sorted.distinct.map(_.fullDescription))
}
