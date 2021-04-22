package org.scalawag.bateman.json.generic.encoding

import cats.syntax.validated._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import org.scalawag.bateman.json.syntax._
import org.scalawag.bateman.json.{DataDrivenTestUtils, NotNull, Null, Nullable, ParserTestUtils}
import OptionTest._
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
//  val jsonAbsent = parseAs[JObject]("""{}""")
//  val jsonNumber = parseAs[JObject]("""{"a": 31}""")
//  val jsonNull = parseAs[JObject]("""{"a": null}""")
//
//  val jsonNullA = jsonNull.fields.getOrElse(fail)("a").value.asNull.getOrElse(fail)

  implicit val aenc = semiauto.deriveEncoderForCaseClass[A]()
  implicit val benc = semiauto.deriveEncoderForCaseClass[B]()
  implicit val cenc = semiauto.deriveEncoderForCaseClass[C]()
  implicit val denc = semiauto.deriveEncoderForCaseClass[D]()
  implicit val eenc = semiauto.deriveEncoderForCaseClass[E]()
  implicit val fenc = semiauto.deriveEncoderForCaseClass[F]()
  implicit val genc = semiauto.deriveEncoderForCaseClass[G]()
  implicit val henc = semiauto.deriveEncoderForCaseClass[H]()
  implicit val ienc = semiauto.deriveEncoderForCaseClass[I]()
  implicit val jenc = semiauto.deriveEncoderForCaseClass[J]()
  implicit val kenc = semiauto.deriveEncoderForCaseClass[K]()
  implicit val lenc = semiauto.deriveEncoderForCaseClass[L]()

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
        actual shouldBe parse(expected).toEncoding
      }(pos)
  }
}
