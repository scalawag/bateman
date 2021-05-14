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

package org.scalawag.bateman.json.decoding

import cats.syntax.validated._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import org.scalawag.bateman.json.validating.{ValidatedCompanion, ValidationFailure, Validator}

class ValidatedCompanionTest extends AnyFunSpec with Matchers {
  it("should use validation") {
    case class String16 private (s: String)

    object String16 extends ValidatedCompanion[String, String16, Any] {
      override implicit val validator: Validator[String, String16] = { in =>
        in.length match {
          case n if n < 1  => ValidationFailure("value must not be empty").invalidNec
          case n if n > 16 => ValidationFailure("value must be sixteen characters or fewer").invalidNec
          case _           => new String16(in).validNec
        }
      }
    }

    val d = JAnyDecoder[String16].decode(JString(""""blah"""", null, null))
    println(d)
  }

  it("should use Validator") {
    case class String16(s: String)

    implicit val string16Validator: Validator[String, String16] = { in =>
      in.length match {
        case n if n < 1  => ValidationFailure("value must not be empty").invalidNec
        case n if n > 16 => ValidationFailure("value must be sixteen characters or fewer").invalidNec
        case _           => String16(in).validNec
      }
    }

    // TODO: I really what this to be totally automatic, but enabling it causes diverging expansion for now.
    implicit val dec: Decoder[JAny, String16] = Decoder[JAny, String].withValidation[String16]

    val d = JAnyDecoder[String16].decode(JString(""""blah"""", null, null))

    println(d)
  }
}
