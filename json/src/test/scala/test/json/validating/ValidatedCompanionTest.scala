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

package test.json.validating

import cats.syntax.either._
import org.scalatest.{EitherValues, Inside}
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import org.scalawag.bateman.json.{InvalidValue, JString}
import org.scalawag.bateman.json.validating.{ValidatedCompanion, ValidationFailure, Validator}

class ValidatedCompanionTest extends AnyFunSpec with Matchers with EitherValues with Inside {
  case class String16 private(s: String)

  object String16 extends ValidatedCompanion[String, String16] {
    override implicit val validator: Validator[String, String16] = { in =>
      in.length match {
        case n if n < 1 => ValidationFailure("value must not be empty").leftNec
        case n if n > 16 => ValidationFailure("value must be sixteen characters or fewer").leftNec
        case _ => new String16(in).rightNec
      }
    }
  }

  it("should pass validation") {
    val d = JString("""blah""").asRootFocus.decode[String16].value
    d.s shouldBe "blah"
  }

  it("should fail validation for too short") {
    val f = JString("").asRootFocus
    val e = f.decode[String16].left.value
    inside(e.iterator.toList) {
      case List(InvalidValue(f, "value must not be empty")) => succeed
    }
  }

  it("should fail validation for too long") {
    val f = JString("blahblahblahblah!").asRootFocus
    val e = f.decode[String16].left.value
    inside(e.iterator.toList) {
      case List(InvalidValue(f, "value must be sixteen characters or fewer")) => succeed
    }
  }
}