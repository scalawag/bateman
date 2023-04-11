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

package test.json.generic.encoding

import org.scalawag.bateman.json.generic.semiauto.unchecked._
import org.scalawag.bateman.json.syntax._
import org.scalawag.bateman.json.{NotNull, Null, Nullable}
import test.json.generic.encoding.OptionTest._
import test.json.{BatemanTestBase, DataDrivenTestUtils}

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

class OptionTest extends BatemanTestBase with DataDrivenTestUtils {
//  val jsonAbsent = parseAs[JObject]("""{}""")
//  val jsonNumber = parseAs[JObject]("""{"a": 31}""")
//  val jsonNull = parseAs[JObject]("""{"a": null}""")
//
//  val jsonNullA = jsonNull.fields.getOrElse(fail)("a").value.asNull.getOrElse(fail)

  implicit val aenc = deriveEncoderForCaseClass[A]()
  implicit val benc = deriveEncoderForCaseClass[B]()
  implicit val cenc = deriveEncoderForCaseClass[C]()
  implicit val denc = deriveEncoderForCaseClass[D]()
  implicit val eenc = deriveEncoderForCaseClass[E]()
  implicit val fenc = deriveEncoderForCaseClass[F]()
  implicit val genc = deriveEncoderForCaseClass[G]()
  implicit val henc = deriveEncoderForCaseClass[H]()
  implicit val ienc = deriveEncoderForCaseClass[I]()
  implicit val jenc = deriveEncoderForCaseClass[J]()
  implicit val kenc = deriveEncoderForCaseClass[K]()
  implicit val lenc = deriveEncoderForCaseClass[L]()

  val cases: Iterable[DataDrivenTestCase[(Any, String)]] = {
    Iterable(
      A(None).toJAny -> s"""{}""",
      A(Some(31)).toJAny -> s"""{"a": 31}""",
      B(None).toJAny -> s"""{}""",
      B(Some(31)).toJAny -> s"""{"a": 31}""",
      C(None).toJAny -> s"""{}""",
      C(Some(7)).toJAny -> s"""{}""",
      C(Some(31)).toJAny -> s"""{"a": 31}""",
      D(31).toJAny -> s"""{"a": 31}""",
      E(31).toJAny -> s"""{"a": 31}""",
      E(7).toJAny -> s"""{}""",
      F(Null).toJAny -> s"""{"a": null}""",
      F(NotNull(31)).toJAny -> s"""{"a": 31}""",
      G(Null).toJAny -> s"""{}""",
      G(NotNull(31)).toJAny -> s"""{"a": 31}""",
      H(Null).toJAny -> s"""{"a": null}""",
      H(NotNull(7)).toJAny -> s"""{}""",
      H(NotNull(31)).toJAny -> s"""{"a": 31}""",
      I(None).toJAny -> s"""{}""",
      I(Some(Null)).toJAny -> s"""{"a": null}""",
      I(Some(NotNull(31))).toJAny -> s"""{"a": 31}""",
      J(None).toJAny -> s"""{}""",
      J(Some(Null)).toJAny -> s"""{"a": null}""",
      J(Some(NotNull(31))).toJAny -> s"""{"a": 31}""",
      K(None).toJAny -> s"""{}""",
      K(Some(Null)).toJAny -> s"""{}""",
      K(Some(NotNull(31))).toJAny -> s"""{"a": 31}""",
      L(None).toJAny -> s"""{}""",
      L(Some(Null)).toJAny -> s"""{"a": null}""",
      L(Some(NotNull(7))).toJAny -> s"""{}""",
      L(Some(NotNull(31))).toJAny -> s"""{"a": 31}""",
    )
  }

  cases.zipWithIndex foreach {
    case (DataDrivenTestCase((actual, expected), pos), n) =>
      it(s"test case #$n") {
        actual shouldBe parse(expected).value.stripLocation
      }(pos)
  }
}
