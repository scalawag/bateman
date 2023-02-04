package org.scalawag.bateman.jsonapi.generic.decoding

import cats.data.Validated
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import org.scalawag.bateman.json.ParserTestUtils
import org.scalawag.bateman.json.decoding.query.as
import org.scalawag.bateman.json.generic.SourceTag
import org.scalawag.bateman.jsonapi.decoding.Document
import org.scalawag.bateman.jsonapi.{decoding, encoding}
import org.scalawag.bateman.jsonapi.generic.{AttributeTag, IdTag, semiauto}
import org.scalawag.bateman.jsonapi.query.{data, required}
import shapeless.tag.@@

class JSourceTest extends AnyFunSpec with Matchers with ParserTestUtils {
  it("should convert to a String") {
    case class AffiniHoodie(id: String @@ IdTag, size: String @@ AttributeTag, limitedEdition: Boolean @@ AttributeTag, color: String @@ AttributeTag, json: Option[JSource] @@ SourceTag = None)
    val codec = semiauto.deriveResourceObjectCodecForCaseClass[AffiniHoodie]("affinihoodie")
    val affiniHoodie = AffiniHoodie(id = "affinifan-78", size = "M", limitedEdition = true, color = "grey")
    val encodingResourceObject = codec.encoder.encode(affiniHoodie)
    val stringifiedHoodie = encoding.ResourceObject.encoder.encode(encodingResourceObject).render
    val decodingResourceObject = parseAs[decoding.ResourceObject](stringifiedHoodie)

    val desiredString =
      """
        |JSource: {
        |  "type": "affinihoodie",
        |  "id": "affinifan-78",
        |  "attributes": {
        |    "color": "grey",
        |    "limitedEdition": true,
        |    "size": "M"
        |  }
        |}
        |""".stripMargin.stripLeading().stripTrailing()

    decodingResourceObject.src.toString shouldBe desiredString
  }
}
