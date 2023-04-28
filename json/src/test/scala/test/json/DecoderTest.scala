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

import cats.syntax.either._
import org.scalacheck.{Arbitrary, Gen}
import org.scalactic.source.Position
import org.scalawag.bateman.json._
import org.scalawag.bateman.json.focus.JFocus

import java.time.{Instant, LocalDate, LocalDateTime, LocalTime}
import java.util.UUID
import scala.reflect.{ClassTag, classTag}
import scala.util.{Failure, Success, Try}
import org.scalawag.bateman.json.focus.weak._

class DecoderTest extends BatemanTestBase {

  describe("identityDecoder") {
    it("should enable no-op decodings") {
      forAll(genJFocus(genJAny)) { f =>
        f.decode[JAny].shouldSucceed shouldBe f.value
      }
    }
  }

  it("JStringDecoder[String]") {
    forAll(genJFocus(genJString)) { f =>
      f.decode[String].shouldSucceed shouldBe f.value.value
    }
  }

  it("JBooleanDecoder[Boolean]") {
    forAll(genJFocus(genJBoolean)) { f =>
      f.decode[Boolean].shouldSucceed shouldBe f.value.value
    }
  }

  def unsafeNumberTestCase[A: Numeric: Arbitrary: ClassTag](
      fn: String => A
  )(implicit decoder: JNumberDecoder[A]): Unit =
    describe(s"JNumberDecoder[${classTag[A]}]") {
      it("should always succeed") {
        forAll(genJFocus(genJNumberOf[A])) { f =>
          decoder.decode(f).shouldSucceed shouldBe fn(f.value.value)
        }
      }

      it("should usually fail") {
        forAll(genJFocus(genJNumber)) {
          case f @ JFocus.Value(JNumber.Value(s)) =>
            Try(fn(s)) match {
              case Success(b) =>
                decoder.decode(f).shouldSucceed shouldBe b
              case Failure(_) =>
                decoder.decode(f) shouldBe InvalidValue(f, s"'${s}' is not a valid ${classTag[A]}").leftNec
            }
          case _ => fail("should never happen")
        }
      }
    }

  unsafeNumberTestCase[Byte](_.toByte)
  unsafeNumberTestCase[Short](_.toShort)
  unsafeNumberTestCase[Int](_.toInt)
  unsafeNumberTestCase[Long](_.toLong)
  unsafeNumberTestCase[Float](_.toFloat)
  unsafeNumberTestCase[Double](_.toDouble)
  unsafeNumberTestCase[BigInt](BigInt(_))
  unsafeNumberTestCase[BigDecimal](BigDecimal.exact)

  def unsafeStringUseCase[A: ClassTag: JStringDecoder](
      gen: Gen[A],
      fn: String => A,
      msg: String => String,
      label: Option[String] = None
  )(implicit pos: Position): Unit =
    describe(s"JStringDecoder[${classTag[A].runtimeClass.getSimpleName}]${label.map(" (" + _ + ")").getOrElse("")}") {
      it("should decode properly") {
        forAll(gen) { a =>
          JString(a.toString).asRootFocus.decode[A].shouldSucceed shouldBe a
        }
      }

      it("should fail to decode when invalid") {
        forAll(genJFocus(genJString)) { f =>
          Try(fn(f.value.value)) match {
            case Success(d) => f.decode[A].shouldSucceed shouldBe d
            case Failure(_) => f.decode[A] shouldBe InvalidValue(f, msg(f.value.value)).leftNec
          }
        }
      }
    }

  unsafeStringUseCase[LocalDate](
    genLocalDate,
    LocalDate.parse,
    x => s"'$x' is not a valid LocalDate (must be in 'yyyy-mm-dd' format)"
  )

  unsafeStringUseCase[LocalTime](
    genLocalTime,
    LocalTime.parse,
    x => s"'$x' is not a valid LocalTime (must be in 'hh:mm:ss' format)"
  )

  unsafeStringUseCase[LocalDateTime](
    genLocalDateTime,
    LocalDateTime.parse,
    x => s"'$x' is not a valid LocalDateTime (must be in 'yyyy-mm-ddThh:mm:ss' format)"
  )

  unsafeStringUseCase[Instant](
    genInstant,
    Instant.parse,
    x => s"'$x' is not a valid Instant (must be in 'yyyy-mm-ddThh:mm:ss[.sss[sss[sss]]]Z' format)"
  )

  unsafeStringUseCase[UUID](
    genUuid,
    UUID.fromString,
    x => s"'$x' is not a valid UUID (must be in 8-4-4-4-12 hex format)"
  )

  describe("JStringDecoder[Char]") {
    it("should decode") {
      forAll(Gen.asciiPrintableChar) { c =>
        val f = JString(c.toString).asRootFocus
        f.decode[Char].shouldSucceed shouldBe c
      }
    }

    it("should decode if length is one") {
      forAll(genJFocus(genJString)) {
        case f @ JFocus.Value(JString.Value(s)) =>
          if (s.length == 1)
            f.decode[Char].shouldSucceed shouldBe s.head
          else
            f.decode[Char].shouldFailSingle shouldBe InvalidValue(f, s"'$s' is not a string of exactly length one")
        case _ => fail("should never happen")
      }
    }
  }

  it("JAnyDecoder[Nullable[A]]") {
    forAll(genJFocus(genJAny)) {
      case f @ JFocus.Value(_: JNull) =>
        f.decode[Nullable[String]].shouldSucceed shouldBe Null
      case f @ JFocus.Value(js: JString) =>
        inside(f.decode[Nullable[String]].shouldSucceed) {
          case NotNull(s: String) =>
            s shouldBe js.value
        }
      case f =>
        f.decode[Nullable[String]] shouldBe JsonTypeMismatch(f, JNull, JString).leftNec
    }
  }

  describe("JStringDecoder[JNumber]") {
    implicit val dec = Decoder.jstringToJNumber

    it("should decode a string as a number") {
      forAll(genJNumber) { n =>
        JString(n.value).asRootFocus.decode[JNumber].shouldSucceed shouldBe n.stripLocation
      }
    }

    it("should fail for non-numeric strings") {
      forAll(genJRootFocus(genJString)) { f =>
        if (JNumber(f.value.value).isLeft)
          f.decode[JNumber].shouldFailSingle shouldBe InvalidValue(f, s"'${f.value.value}' is not a valid JSON number")
        else
          succeed
      }
    }
  }

  describe("JObjectDecoder[Map[A, B]]") {
    it("should decode object to map") {
      forAll(genJFocus(genJObject)) { f =>
        val out = f.decode[Map[String, JAny]].shouldSucceed

        out.size shouldBe f.value.fieldList.map(_.name).distinct.length
        out.foreach {
          case (k, v) =>
            f.value.fieldList.filter(_.name.value == k).lastOption.map(_.value) shouldBe Some(v)
        }
      }
    }

    it("should decode with specific decoders") {
      implicit val keyDecoder: Decoder[JString, Int] =
        Decoder.jstringToJNumber.andThen(Decoder.jnumberToIntDecoder)

      val genJObjectWithIntegerKeys = {
        val genInt = Gen.choose(0, 99999) // arbitrary limit here

        val genField = for {
          k <- genInt
          v <- genInt
        } yield k.toString -> JNumber(v)

        for {
          len <- Gen.choose(0, 8)
          ff <- Gen.listOfN(len, genField)
        } yield JObject(ff: _*)
      }

      forAll(genJFocus(genJObjectWithIntegerKeys)) { f =>
        val out = f.decode[Map[Int, Int]].shouldSucceed

        out.size shouldBe f.value.fieldList.map(_.name).distinct.length
        out.foreach {
          case (k, v) =>
            inside(f.value.fieldList.filter(_.name.value.toInt == k).lastOption.map(_.value)) {
              case Some(JNumber.Value(n)) => n shouldBe v.toString
            }
        }
      }
    }
  }

  describe("JArrayDecoder[A]") {
    it("should decode array to list") {
      forAll(genJFocus(genJArray)) { f =>
        val out = f.decode[List[JAny]].shouldSucceed
        out shouldBe f.value.items
      }
    }

    it("should decode to specific types") {
      val f = JArray(JNumber(17), JNumber(23), JNull).asRootFocus
      val out = f.decode[List[Nullable[Int]]].shouldSucceed
      out shouldBe List(NotNull(17), NotNull(23), Null)
    }
  }
}
