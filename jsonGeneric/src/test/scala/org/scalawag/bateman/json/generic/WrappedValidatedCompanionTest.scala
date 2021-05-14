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
import org.scalawag.bateman.json.decoding.{InvalidValue, JAny, JLocation, JNumber, JPointer}
import org.scalawag.bateman.json.syntax._
import org.scalawag.bateman.json.generic.WrappedValidatedCompanionTest.NonNegativeInteger
import org.scalawag.bateman.json.validating.{ValidationFailure, WrappedValidatedCompanion}

object WrappedValidatedCompanionTest {
  final case class NonNegativeInteger private[NonNegativeInteger] (value: Int)

  object NonNegativeInteger extends WrappedValidatedCompanion[Int, NonNegativeInteger, Any](new NonNegativeInteger(_)) {
    override def validate(in: Int): List[String] =
      if (in < 0)
        List("has to be greater than zero")
      else
        Nil
  }
}

class WrappedValidatedCompanionTest extends AnyFunSpec with Matchers {
  private val neg: JAny = JNumber("-1", JLocation(1, 1), JPointer.Root)
  private val pos: JAny = JNumber("1", JLocation(1, 1), JPointer.Root)

  it("should accept NonNegativeInteger on decode") {
    pos.as[NonNegativeInteger] shouldBe NonNegativeInteger.unsafe(1).validNec
  }

  it("should accept NonNegativeInteger on apply") {
    NonNegativeInteger(1) shouldBe NonNegativeInteger.unsafe(1).validNec
  }

  it("should accept NonNegativeInteger on unsafe") {
    NonNegativeInteger.unsafe(1) shouldBe NonNegativeInteger.unsafe(1)
  }

  it("should reject NonNegativeInteger on decode") {
    neg.as[NonNegativeInteger] shouldBe InvalidValue(neg, "has to be greater than zero").invalidNec
  }

  it("should reject NonNegativeInteger on apply") {
    NonNegativeInteger(-1) shouldBe ValidationFailure("has to be greater than zero").invalidNec
  }

  it("should reject NonNegativeInteger on unsafe") {
    val ex = intercept[IllegalArgumentException] {
      NonNegativeInteger.unsafe(-1)
    }
    ex.getMessage should include("has to be greater than zero")
  }
}
