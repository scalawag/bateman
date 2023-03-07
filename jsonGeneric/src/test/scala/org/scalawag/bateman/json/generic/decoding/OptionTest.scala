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

package org.scalawag.bateman.json.generic.decoding

import cats.syntax.validated._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import org.scalawag.bateman.json.syntax._
import org.scalawag.bateman.json.{DataDrivenTestUtils, NotNull, Null, Nullable, ParserTestUtils}
import OptionTest._
import org.scalawag.bateman.json.decoding.{JNumber, JObject, JsonTypeMismatch, UnspecifiedField}
import org.scalawag.bateman.json.generic.semiauto

object OptionTest {
  final case class A(a: Option[Int])
  final case class B(a: Option[Int] = None)
  final case class C(a: Option[Int] = Some(7))
  final case class D(a: Int)
  final case class E(a: Int = 7)
  final case class F(a: Nullable[Int])
  final case class G(a: Nullable[Int] = Null)
  final case class H(a: Nullable[Int] = NotNull(7))
  final case class I(a: Option[Nullable[Int]])
  final case class J(a: Option[Nullable[Int]] = None)
  final case class K(a: Option[Nullable[Int]] = Some(Null))
  final case class L(a: Option[Nullable[Int]] = Some(NotNull(7)))
}

class OptionTest extends AnyFunSpec with Matchers with ParserTestUtils with DataDrivenTestUtils {
  val jsonAbsent = parseAs[JObject]("""{}""")
  val jsonNumber = parseAs[JObject]("""{"a": 31}""")
  val jsonNull = parseAs[JObject]("""{"a": null}""")

  val jsonNullA = jsonNull.fields.getOrElse(fail())("a").value.asNull.getOrElse(fail())

  implicit val adec = semiauto.deriveDecoderForCaseClass[A, Any]()
  implicit val bdec = semiauto.deriveDecoderForCaseClass[B, Any]()
  implicit val cdec = semiauto.deriveDecoderForCaseClass[C, Any]()
  implicit val ddec = semiauto.deriveDecoderForCaseClass[D, Any]()
  implicit val edec = semiauto.deriveDecoderForCaseClass[E, Any]()
  implicit val fdec = semiauto.deriveDecoderForCaseClass[F, Any]()
  implicit val gdec = semiauto.deriveDecoderForCaseClass[G, Any]()
  implicit val hdec = semiauto.deriveDecoderForCaseClass[H, Any]()
  implicit val idec = semiauto.deriveDecoderForCaseClass[I, Any]()
  implicit val jdec = semiauto.deriveDecoderForCaseClass[J, Any]()
  implicit val kdec = semiauto.deriveDecoderForCaseClass[K, Any]()
  implicit val ldec = semiauto.deriveDecoderForCaseClass[L, Any]()

  val cases: Iterable[DataDrivenTestCase[(Any, Any)]] = {
    Iterable(
      jsonAbsent.as[A] -> UnspecifiedField(jsonAbsent, "a").invalidNec,
      jsonAbsent.as[B] -> B(None).validNec,
      jsonAbsent.as[C] -> C(Some(7)).validNec,
      jsonAbsent.as[D] -> UnspecifiedField(jsonAbsent, "a").invalidNec,
      jsonAbsent.as[E] -> E(7).validNec,
      jsonAbsent.as[F] -> UnspecifiedField(jsonAbsent, "a").invalidNec,
      jsonAbsent.as[G] -> G(Null).validNec,
      jsonAbsent.as[H] -> H(NotNull(7)).validNec,
      jsonAbsent.as[I] -> UnspecifiedField(jsonAbsent, "a").invalidNec,
      jsonAbsent.as[J] -> J(None).validNec,
      jsonAbsent.as[K] -> K(Some(Null)).validNec,
      jsonAbsent.as[L] -> L(Some(NotNull(7))).validNec,
      jsonNumber.as[A] -> A(Some(31)).validNec,
      jsonNumber.as[B] -> B(Some(31)).validNec,
      jsonNumber.as[C] -> C(Some(31)).validNec,
      jsonNumber.as[D] -> D(31).validNec,
      jsonNumber.as[E] -> E(31).validNec,
      jsonNumber.as[F] -> F(NotNull(31)).validNec,
      jsonNumber.as[G] -> G(NotNull(31)).validNec,
      jsonNumber.as[H] -> H(NotNull(31)).validNec,
      jsonNumber.as[I] -> I(Some(NotNull(31))).validNec,
      jsonNumber.as[J] -> J(Some(NotNull(31))).validNec,
      jsonNumber.as[K] -> K(Some(NotNull(31))).validNec,
      jsonNumber.as[L] -> L(Some(NotNull(31))).validNec,
      jsonNull.as[A] -> JsonTypeMismatch(jsonNullA, JNumber).invalidNec,
      jsonNull.as[B] -> JsonTypeMismatch(jsonNullA, JNumber).invalidNec,
      jsonNull.as[C] -> JsonTypeMismatch(jsonNullA, JNumber).invalidNec,
      jsonNull.as[D] -> JsonTypeMismatch(jsonNullA, JNumber).invalidNec,
      jsonNull.as[E] -> JsonTypeMismatch(jsonNullA, JNumber).invalidNec,
      jsonNull.as[F] -> F(Null(jsonNullA)).validNec,
      jsonNull.as[G] -> G(Null(jsonNullA)).validNec,
      jsonNull.as[H] -> H(Null(jsonNullA)).validNec,
      jsonNull.as[I] -> I(Some(Null(jsonNullA))).validNec,
      jsonNull.as[J] -> J(Some(Null(jsonNullA))).validNec,
      jsonNull.as[K] -> K(Some(Null(jsonNullA))).validNec,
      jsonNull.as[L] -> L(Some(Null(jsonNullA))).validNec,
    )
  }

  cases.zipWithIndex foreach {
    case (DataDrivenTestCase((actual, expected), pos), n) =>
      it(s"test case #$n") {
        actual shouldBe expected
      }(pos)
  }
}
