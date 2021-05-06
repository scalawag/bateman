package org.scalawag.bateman.json.generic

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import org.scalawag.bateman.json.generic.naming.CamelCase

class NamingTest extends AnyFunSpec with Matchers {
  describe("CamelCase") {
    val cases =
      Iterable(
        "address1" -> List("address1"),
        "uuid" -> List("uuid"),
        "UUID" -> List("u", "u", "i", "d"),
        "base64uuid" -> List("base64uuid"),
        "base64Uuid" -> List("base64", "uuid"),
        "base64UUID" -> List("base64", "u", "u", "i", "d"),
        "ZNamedClass" -> List("z", "named", "class")
      )

    cases.foreach {
      case (in, out) =>
        it(s"should convert '$in' to words") {
          CamelCase.toWords(in) shouldBe out
        }
    }
  }

  describe("CamelCase with capital grouping") {
    val cases =
      Iterable(
        "address1" -> List("address1"),
        "uuid" -> List("uuid"),
        "UUID" -> List("uuid"),
        "base64uuid" -> List("base64uuid"),
        "base64Uuid" -> List("base64", "uuid"),
        "base64UUID" -> List("base64", "uuid"),
        "ZNamedClass" -> List("znamed", "class")
      )

    cases.foreach {
      case (in, out) =>
        it(s"should convert '$in' to words") {
          CamelCase(true).toWords(in) shouldBe out
        }
    }
  }
}
