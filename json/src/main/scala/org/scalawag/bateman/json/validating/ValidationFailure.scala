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

package org.scalawag.bateman.json.validating

import cats.data.NonEmptyChain
import cats.data.ValidatedNec
import org.scalawag.bateman.json.JPointer

/** Represents a failure to validate a given value. This supports nested validation, so it's possible to gather all
  * the validation failures of the individual elements of a JSON structure and present them as the failures of the
  * containing structure. This is intentionally not specific to decoding, which already has a way to represent the
  * structure of the value being decoded. It allows factory methods for validated types to return deep failures as
  * part of an exception message thrown during instantiation.
  *
  * @param description the reason for the validation
  * @param pointer a pointer to the value that failed validation
  */

case class ValidationFailure(description: String, pointer: JPointer = JPointer.Root) {
  lazy val fullDescription: String = pointer match {
    case JPointer.Root => description
    case p             => s"$p: $description"
  }
}

object ValidationFailure {

  /** Throws an IllegalArgumentException containing a human-readable failure report. This is designed to be used with
    * [[ValidatedNec]].fold */
  def throwValidationErrors(failures: NonEmptyChain[ValidationFailure]): Nothing =
    throw new ValidationFailedException(failures)

  /** Formats validation failures as a human-readable string. */
  def formatFailures(errors: NonEmptyChain[ValidationFailure]): String =
    errors.map(_.fullDescription).iterator.mkString("argument failed validation:\n - ", "\n - ", "")
}
