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

import cats.syntax.validated._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import org.scalawag.bateman.json.decoding.{InvalidValue, JAnyDecoder, JString, JPointer, JLocation}
import org.scalawag.bateman.json.validating.ValidationFailure
import shapeless.tag.@@

class TaggedValidatedCompanionTest extends AnyFunSpec with Matchers {
  trait NonEmptyString

  object NonEmptyString extends TaggedValidation[String, NonEmptyString, Any] {
    override def validate(in: String): List[String] =
      in.length match {
        case n if n < 1 => List("value must not be empty")
        case _          => Nil
      }
  }

  it("should fail empty string with apply") {
    NonEmptyString("") shouldBe ValidationFailure("value must not be empty").invalidNec
  }

  it("should pass non-empty string with apply") {
    NonEmptyString("XXX") shouldBe "XXX".validNec
  }

  it("should fail empty string with unsafe") {
    val ex = intercept[IllegalArgumentException] {
      NonEmptyString.unsafe("")
    }
    ex.getMessage should include("value must not be empty")
  }

  it("should pass non-empty string with unsafe") {
    NonEmptyString.unsafe("XXX") shouldBe "XXX"
  }

  it("should fail empty string with decode") {
    val v = JString("", JLocation(1, 1), JPointer.Root)
    JAnyDecoder[String @@ NonEmptyString].decode(v) shouldBe InvalidValue(v, "value must not be empty").invalidNec
  }

  it("should pass non-empty string with decode") {
    val v = JString("XXX", JLocation(1, 1), JPointer.Root)
    JAnyDecoder[String @@ NonEmptyString].decode(v) shouldBe "XXX".valid
  }
}
