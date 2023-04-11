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

package test.json

import cats.syntax.contravariant._
import org.scalacheck.Arbitrary
import org.scalactic.TolerantNumerics
import org.scalawag.bateman.json._
import org.scalawag.bateman.json.syntax._

import java.time.{Instant, LocalDate, LocalDateTime, LocalTime}
import java.util.UUID
import scala.reflect.{ClassTag, classTag}

class EncoderTest extends BatemanTestBase {

  describe("identityEncoder") {
    it("should enable no-op encodings") {
      forAll(genJAny) { f =>
        f.toJAny shouldBe f
      }
    }
  }

  it("JBooleanEncoder[Boolean]") {
    forAll { (b: Boolean) =>
      b.toJAny shouldBe JBoolean(b)
    }
  }

  it("JNullEncoder[Null]") {
    Null.toJAny shouldBe JNull
  }

  def stringTestCase[A: ClassTag: Arbitrary: JStringEncoder](): Unit =
    it(s"JStringEncoder[${classTag[A]}]") {
      forAll { (a: A) =>
        a.toJAny shouldBe JString(a.toString)
      }
    }

  stringTestCase[String]()
  stringTestCase[Char]()
  stringTestCase[UUID]()
  stringTestCase[LocalDate]()
  stringTestCase[LocalTime]()
  stringTestCase[LocalDateTime]()
  stringTestCase[Instant]()

  def numberTestCase[A: ClassTag: Arbitrary: JNumberEncoder](): Unit =
    it(s"JNumberEncoder[${classTag[A]}]") {
      forAll { (a: A) =>
        // Use === to handle the imprecision in the float and double types
        a.toJAny === JNumber.unsafe(a.toString)
      }
    }

  numberTestCase[Byte]()
  numberTestCase[Short]()
  numberTestCase[Int]()
  numberTestCase[Long]()
  numberTestCase[Float]()
  numberTestCase[Double]()
  numberTestCase[BigInt]()
  numberTestCase[BigDecimal]()

  it("JAnyEncoder[Nullable[A]]") {
    forAll { (oa: Option[String]) =>
      val na = Nullable(oa)
      na match {
        case Null       => na.toJAny shouldBe JNull
        case NotNull(a) => na.toJAny shouldBe a.toJAny
      }
    }
  }

  describe("JObjectEncoder[Map[A, B]]") {
    it("should encode object to map") {
      val genMapNoDups = Arbitrary.arbitrary[Map[String, String]].retryUntil(m => m.keySet.size == m.size)
      forAll(genMapNoDups) { a =>
        val out = a.toJAny
        out.fieldList.length shouldBe a.size
        out.fieldList.foreach {
          case JField(k, v: JString) =>
            v.value shouldBe a(k.value)
        }
      }
    }

    it("should encode with specific key encoder") {
      val genMapNoDups = Arbitrary.arbitrary[Map[Int, Long]].retryUntil(m => m.keySet.size == m.size)
      implicit val keyEncoder: JStringEncoder[Int] = JStringEncoder[String].contramap(_.toString)
      forAll(genMapNoDups) { a =>
        val out = a.toJAny
        out.fieldList.length shouldBe a.size
        out.fieldList.foreach {
          case JField(k, v: JNumber) =>
            v.value.toLong shouldBe a(k.value.toInt)
        }
      }
    }
  }

  describe("JArrayEncoder[") {
    it("should encode empty array to empty list") {
      List.empty[JAny].toJAny shouldBe JArray.Empty
    }

    it("should encode array to list") {
      forAll { (a: List[BigDecimal]) =>
        a.toJAny shouldBe JArray(a.map(_.toJAny): _*)
      }
    }
  }
}
