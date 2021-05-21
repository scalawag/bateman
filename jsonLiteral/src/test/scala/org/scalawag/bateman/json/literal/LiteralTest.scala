package org.scalawag.bateman.json.literal

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import org.scalawag.bateman.json.encoding.{JAnyEncoder, JArray, JBoolean, JNull, JNumber, JObject, JString}
import java.net.URL

class LiteralTest extends AnyFunSpec with Matchers {
  it("should parse JSON text with no variables") {
    val json = json"""
      {
        "a": 4,
        "b": true,
        "c": null,
        "d": [
          { "a": 8 },
          [1,2,3],
          "my\\\":{}[]()*_-12",
          true,
          false,
          null,
          []
        ]
      }
    """

    json shouldBe JObject(
      "a" -> JNumber(4),
      "b" -> JBoolean(true),
      "c" -> JNull,
      "d" -> JArray(
        JObject("a" -> JNumber(8)),
        JArray(JNumber(1), JNumber(2), JNumber(3)),
        JString("my\\\":{}[]()*_-12"),
        JBoolean(true),
        JBoolean(false),
        JNull,
        JArray()
      )
    )
  }

  it("should parse JSON text with interpolations") {
    val string = "what \"about\" escape\\sequences? 🤷"

    val json =
      json"""
        {
          "blah": $string,
          "6": 6,
          "C": ${List(0, 1, 4, 3, 5)},
          "D": ${false}
        }
      """

    json shouldBe JObject(
      "blah" -> JString("what \"about\" escape\\sequences? 🤷"),
      "6" -> JNumber(6),
      "C" -> JArray(List(0, 1, 4, 3, 5).map(JNumber(_)): _*),
      "D" -> JBoolean(false)
    )
  }

  it("should disallow any insertion without an encoder") {
    val url = new URL("http://www.example.com/")

    assertTypeError("json\"\"\"{\"foo\":$url}\"\"\"")
  }

  it("should allow any insertion that has an encoder") {
    val url = new URL("http://www.example.com/")

    implicit val urlEncoder: JAnyEncoder[URL] = JAnyEncoder[String].contramap(_.toString)

    val json =
      json"""
        {
          "foo": $url
        }
      """

    json shouldBe JObject("foo" -> JString(url.toString))
  }
}
