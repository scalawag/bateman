package org.scalawag.bateman.jsonapi

import cats.data.Validated
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import org.scalawag.bateman.jsonapi.decoding.{Attributes, Document, Errors, ResourceObjectData}
import org.scalawag.bateman.json.decoding.query._
import org.scalawag.bateman.json.ParserTestUtils
import org.scalawag.bateman.json.decoding.query.Query._

class ToStringTest extends AnyFunSpec with Matchers with ParserTestUtils {
  it("converts a ResourceObject to a String") {
    val d = parseAs[Document]("""
      {
        "data": {
          "type": "my_class",
          "id": "ID",
          "attributes": {
            "a": "A",
            "b": "B"
          }
        }
      }
    """)

    val resourceObject = d.data.collect { case ResourceObjectData(_, r) => r }.getOrElse(fail("Document in ResourceObject toString test was not a ResourceObject."))

    val desiredString = """
      |ResourceObject: {
      |  "type": "my_class",
      |  "id": "ID",
      |  "attributes": {
      |    "a": "A",
      |    "b": "B"
      |  }
      |}
      |""".stripMargin.stripLeading().stripTrailing()

    resourceObject.toString() shouldBe desiredString
    resourceObject.toEncoding.toString() shouldBe desiredString
  }

  it("converts a Document to a String") {
    val d = parseAs[Document](
      """
      {
        "data": {
          "type": "my_class",
          "id": "ID",
          "attributes": {
            "a": "A",
            "b": "B"
          }
        }
      }
    """)

    val desiredString =
      """
        |Document: {
        |  "data": {
        |    "type": "my_class",
        |    "id": "ID",
        |    "attributes": {
        |      "a": "A",
        |      "b": "B"
        |    }
        |  }
        |}
        |""".stripMargin.stripLeading().stripTrailing()

    d.toString() shouldBe desiredString
    d.toEncoding.toString() shouldBe desiredString
  }

  it("converts Attributes to a String") {
    val d = parseAs[Document]("""
      {
        "data": {
          "type": "my_class",
          "id": "ID",
          "attributes": {
            "a": "A",
            "b": "B"
          }
        }
      }
    """)

    val desiredString =
      """
        |Attributes: {
        |  "a": "A",
        |  "b": "B"
        |}
        |""".stripMargin.stripLeading().stripTrailing()


    d.src.root.query(_ ~> "data" ~> "attributes" ~> as[Attributes]) match {
      case Validated.Valid(attributes) =>
        attributes.toString shouldBe desiredString

      case Validated.Invalid(e) => fail(s"Document in Attributes toString test did not contain data.attributes: $e")
    }
  }

  it("converts Errors to a String") {
    val d = parseAs[Document]("""
      {
        "errors": [
          {
            "status": "400",
            "code": "invalid_include_path",
            "detail": "The include path 'hello' is invalid.",
            "source": {
              "parameter": "include"
            },
            "meta": {
              "path": "hello"
            }
          },
          {
            "status": "400",
            "code": "invalid_include_path",
            "detail": "The include path 'world' is invalid.",
            "source": {
              "parameter": "include"
            },
            "meta": {
              "path": "hello"
            }
          }
        ]
      }
    """)

    val desiredString =
      """
        |Errors: [
        |  {
        |    "status": "400",
        |    "code": "invalid_include_path",
        |    "detail": "The include path 'hello' is invalid.",
        |    "source": {
        |      "parameter": "include"
        |    },
        |    "meta": {
        |      "path": "hello"
        |    }
        |  },
        |  {
        |    "status": "400",
        |    "code": "invalid_include_path",
        |    "detail": "The include path 'world' is invalid.",
        |    "source": {
        |      "parameter": "include"
        |    },
        |    "meta": {
        |      "path": "hello"
        |    }
        |  }
        |]
        |""".stripMargin.stripLeading().stripTrailing()

    d.src.root.query(_ ~> "errors" ~> as[Errors]) match {
      case Validated.Valid(errors) =>
        errors.toString() shouldBe desiredString

      case Validated.Invalid(e) => fail("Document in Errors toString test was not Errors")
    }
  }
}
