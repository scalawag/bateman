package org.scalawag.bateman.json.generic.encoding

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import org.scalawag.bateman.json.decoding.{DecodeError, JAnyDecoder}
import org.scalawag.bateman.json.encoding.{JNull, JObject, JObjectEncoder, JString}
import org.scalawag.bateman.json.generic.encoding.HListEncoderTest.WithDefaults
import org.scalawag.bateman.json.generic.{Config, semiauto}
import org.scalawag.bateman.json.syntax._
import org.scalawag.bateman.json.{NotNull, Null, Nullable}

object HListEncoderTest {
  val mydef: Nullable[String] = parse[Nullable[String]]("null")

  case class WithDefaults(
      a: Nullable[Int] = Null,
      b: Nullable[String] = mydef //NotNull("foo")
  )

  def parse[A](json: String)(implicit decoder: JAnyDecoder[A]): A =
    org.scalawag.bateman.json
      .parse(json)
      .getOrElse(???)
      .as[A]
      .getOrElse(???)
}

class HListEncoderTest extends AnyFunSpec with Matchers {
  it("Should omit default values") {
    implicit val enc: CaseClassEncoder[WithDefaults] = semiauto.deriveEncoderForCaseClass[WithDefaults]()

    JObjectEncoder.encode(WithDefaults(b = Null)) shouldBe JObject()
  }

  it("Should not omit default values") {
    implicit val config: Config = Config.default.copy(encodeDefaultValues = true)
    implicit val enc: CaseClassEncoder[WithDefaults] = semiauto.deriveEncoderForCaseClass[WithDefaults]()

    JObjectEncoder.encode(WithDefaults()) shouldBe JObject("a" -> JNull, "b" -> JNull)
  }
}
