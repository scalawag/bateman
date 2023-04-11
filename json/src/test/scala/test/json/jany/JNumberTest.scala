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

package test.json.jany

import org.scalawag.bateman.json.JNumber
import test.json.BatemanTestBase
import test.json.parser.tokenizer.NumberCharCollectorTest

class JNumberTest extends BatemanTestBase {
  describe("apply") {
    it("should create a number") {
      JNumber("4.37").map(_.value) shouldBe Right("4.37")
    }

    it("should reject an invalid number") {
      JNumber("4..37") shouldBe Left("'4..37' is not a valid JSON number")
    }

    it("should create a number safely from a Long") {
      JNumber(3000000000000L).value shouldBe "3000000000000"
    }

    it("should create a number safely from an Int") {
      JNumber(17).value shouldBe "17"
    }

    it("should create a number safely from a Short") {
      JNumber(145: Short).value shouldBe "145"
    }

    it("should create a number safely from a Byte") {
      JNumber(23: Byte).value shouldBe "23"
    }

    it("should create a number safely from a Double") {
      JNumber(1.1).value shouldBe "1.1"
    }

    it("should create a number safely from a BigInt") {
      JNumber(BigInt(1234567890)).value shouldBe "1234567890"
    }

    it("should create a number safely from a BigDecimal") {
      JNumber(BigDecimal("1234567890.1234567890")).value shouldBe "1234567890.1234567890"
    }

    it("should accept on unsafe with good input") {
      JNumber.unsafe("178").value shouldBe "178"
    }

    it("should throw on unsafe with bad input") {
      val ex = intercept[IllegalArgumentException] {
        JNumber.unsafe("1..0")
      }
      ex.getMessage shouldBe s"'1..0' is not a valid JSON number"
    }

    // Steal negative test cases from NumberCharCollectorTest
    (new NumberCharCollectorTest).cases.collect {
      case (in, Left(_)) =>
        it(s"should reject $in") {
          JNumber(in) shouldBe Left(s"'$in' is not a valid JSON number")
        }
    }

    // Steal positive test cases from NumberCharCollectorTest
    (new NumberCharCollectorTest).cases
      .collect {
        case (in, Right(n)) => in.take(n)
      }
      .toList
      .distinct
      .foreach { in =>
        it(s"should accept ${truncate(in)}") {
          JNumber(in).map(_.value) shouldBe Right(in)
        }

        it(s"should return the BigDecimal for ${truncate(in)}") {
          JNumber.unsafe(in).toBigDecimal shouldBe BigDecimal(in)
        }
      }
  }

  describe("unapply") {
    it("should extract just the value with JNumber.Value") {
      inside(JNumber(808)) {
        case JNumber.Value(s) => s shouldBe "808"
      }
    }
  }
}
