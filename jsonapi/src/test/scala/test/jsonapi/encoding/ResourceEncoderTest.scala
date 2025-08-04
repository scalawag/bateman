// bateman -- Copyright 2021-2026 -- Justin Patterson
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

package test.jsonapi.encoding

import cats.syntax.either._
import org.scalawag.bateman.json._
import org.scalawag.bateman.json.literal._
import org.scalawag.bateman.json.syntax._
import org.scalawag.bateman.jsonapi.encoding._
import org.scalawag.bateman.jsonapi.encoding.ResourceEncoder.Encoded
import test.json.BatemanTestBase

object ResourceEncoderTest {
  case class TestResource(id: String, name: String)

  def makeEncoder(result: EncodeResult[Encoded]): ResourceEncoder[TestResource] =
    new ResourceEncoder[TestResource] {
      override def encodeResource(
          in: TestResource,
          includeSpec: IncludeSpec,
          fieldsSpec: FieldsSpec,
          discriminators: JObject
      ): EncodeResult[Encoded] = result
    }

  val simpleRoot: JObject = json"""{"type": "test", "id": "1", "attributes": {"name": "hello"}}"""
  val simpleEncoded: Encoded = Encoded(simpleRoot)
}

class ResourceEncoderTest extends BatemanTestBase {
  import ResourceEncoderTest._

  describe("encodeInfallibly") {

    it("should return Encoded when encodeResource succeeds") {
      implicit val enc: ResourceEncoder[TestResource] = makeEncoder(simpleEncoded.asRight)
      val result = enc.encodeInfallibly(TestResource("1", "hello"))
      result.root shouldBe simpleRoot
    }

    it("should throw ProgrammerError when encodeResource fails") {
      implicit val enc: ResourceEncoder[TestResource] =
        makeEncoder(InvalidIncludePath("bad").leftNec)
      a[ProgrammerError] should be thrownBy {
        enc.encodeInfallibly(TestResource("1", "hello"))
      }
    }

    it("should pass parameters through") {
      val discs = JObject("disc" -> "value".toJAny)
      implicit val enc: ResourceEncoder[TestResource] = new ResourceEncoder[TestResource] {
        override def encodeResource(
            in: TestResource,
            includeSpec: IncludeSpec,
            fieldsSpec: FieldsSpec,
            discriminators: JObject
        ): EncodeResult[Encoded] = {
          includeSpec shouldBe IncludeSpec.Never
          fieldsSpec shouldBe FieldsSpec.None
          discriminators shouldBe discs
          simpleEncoded.asRight
        }
      }
      enc.encodeInfallibly(TestResource("1", "hello"), IncludeSpec.Never, FieldsSpec.None, discs)
    }
  }

  describe("encodeMinimally") {

    it("should use IncludeSpec.Never and FieldsSpec.None") {
      implicit val enc: ResourceEncoder[TestResource] = new ResourceEncoder[TestResource] {
        override def encodeResource(
            in: TestResource,
            includeSpec: IncludeSpec,
            fieldsSpec: FieldsSpec,
            discriminators: JObject
        ): EncodeResult[Encoded] = {
          includeSpec shouldBe IncludeSpec.Never
          fieldsSpec shouldBe FieldsSpec.None
          simpleEncoded.asRight
        }
      }
      enc.encodeMinimally(TestResource("1", "hello"))
    }

    it("should return only the root JObject") {
      val incObj = json"""{"type": "other", "id": "2"}"""
      val encoded = Encoded(simpleRoot, Inclusions(incObj))
      implicit val enc: ResourceEncoder[TestResource] = makeEncoder(encoded.asRight)
      val result = enc.encodeMinimally(TestResource("1", "hello"))
      result shouldBe simpleRoot
    }

    it("should pass discriminators through") {
      val discs = JObject("extra" -> "val".toJAny)
      implicit val enc: ResourceEncoder[TestResource] = new ResourceEncoder[TestResource] {
        override def encodeResource(
            in: TestResource,
            includeSpec: IncludeSpec,
            fieldsSpec: FieldsSpec,
            discriminators: JObject
        ): EncodeResult[Encoded] = {
          discriminators shouldBe discs
          simpleEncoded.asRight
        }
      }
      enc.encodeMinimally(TestResource("1", "hello"), discs)
    }
  }

  describe("encode (JObjectEncoder override)") {

    it("should use IncludeSpec.Never and FieldsSpec.All") {
      implicit val enc: ResourceEncoder[TestResource] = new ResourceEncoder[TestResource] {
        override def encodeResource(
            in: TestResource,
            includeSpec: IncludeSpec,
            fieldsSpec: FieldsSpec,
            discriminators: JObject
        ): EncodeResult[Encoded] = {
          includeSpec shouldBe IncludeSpec.Never
          fieldsSpec shouldBe FieldsSpec.All
          simpleEncoded.asRight
        }
      }
      enc.encode(TestResource("1", "hello"), JObject.Empty)
    }

    it("should return the root JObject") {
      implicit val enc: ResourceEncoder[TestResource] = makeEncoder(simpleEncoded.asRight)
      val result = enc.encode(TestResource("1", "hello"), JObject.Empty)
      result shouldBe simpleRoot
    }
  }

  describe("ResourceEncoder companion") {

    it("should summon implicit encoder with apply") {
      implicit val enc: ResourceEncoder[TestResource] = makeEncoder(simpleEncoded.asRight)
      ResourceEncoder[TestResource] shouldBe enc
    }

    it("should delegate via companion encodeResource method") {
      implicit val enc: ResourceEncoder[TestResource] = makeEncoder(simpleEncoded.asRight)
      val result = ResourceEncoder.encodeResource(TestResource("1", "hello"))
      result shouldBe simpleEncoded.asRight
    }
  }

  describe("Encoded") {

    it("should transform root via map") {
      val encoded = Encoded(simpleRoot)
      val mapped = encoded.map(_ => JObject("transformed" -> true.toJAny))
      mapped.root.fieldList.head.name.value shouldBe "transformed"
    }

    it("should preserve inclusions in map") {
      val incObj = json"""{"type": "other", "id": "2"}"""
      val inclusions = Inclusions(incObj)
      val encoded = Encoded(simpleRoot, inclusions)
      val mapped = encoded.map(identity)
      mapped.inclusions.objects.toList should have size 1
    }

    it("should produce document with data and included via toDocument") {
      val incObj = json"""{"type": "other", "id": "2"}"""
      val encoded = Encoded(simpleRoot, Inclusions(incObj))
      val doc = encoded.toDocument
      doc.fieldList.map(_.name.value) should contain("data")
      doc.fieldList.map(_.name.value) should contain("included")
    }

    it("should produce empty included array when no inclusions") {
      val encoded = Encoded(simpleRoot, Inclusions.empty)
      val doc = encoded.toDocument
      doc.fieldList.map(_.name.value) should contain("included")
    }
  }
}
