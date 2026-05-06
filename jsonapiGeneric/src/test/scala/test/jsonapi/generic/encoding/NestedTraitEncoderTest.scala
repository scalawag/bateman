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

package test.jsonapi.generic.encoding

import org.scalawag.bateman.json.JString
import org.scalawag.bateman.json.generic.Discriminators._
import org.scalawag.bateman.json.lens._
import org.scalawag.bateman.jsonapi.encoding.ResourceEncoder
import org.scalawag.bateman.jsonapi.generic.Annotations._
import org.scalawag.bateman.jsonapi.generic.semiauto._
import org.scalawag.bateman.jsonapi.lens._
import org.scalawag.bateman.jsonapi.syntax._
import test.json.BatemanTestBase

object NestedTraitEncoderTest {
  // A multi-level sealed hierarchy where leaf types are nested under intermediate sealed traits.
  // After encoder flattening, callers may map leaves directly even when intermediate sealed traits
  // sit between the trait being derived and its concrete subtypes.
  sealed trait Top
  final case class DirectLeaf(@Id id: String, @Attribute a: Int) extends Top

  sealed trait Mid extends Top
  final case class MidLeaf(@Id id: String, @Attribute b: String) extends Mid

  sealed trait Deeper extends Mid
  final case class DeepestLeaf(@Id id: String, @Attribute c: Boolean) extends Deeper

  implicit val directLeafEncoder: ResourceEncoder[DirectLeaf] =
    deriveResourceEncoderForCaseClass[DirectLeaf]("thing")
  implicit val midLeafEncoder: ResourceEncoder[MidLeaf] =
    deriveResourceEncoderForCaseClass[MidLeaf]("thing")
  implicit val deepestLeafEncoder: ResourceEncoder[DeepestLeaf] =
    deriveResourceEncoderForCaseClass[DeepestLeaf]("thing")
}

class NestedTraitEncoderTest extends BatemanTestBase {
  import NestedTraitEncoderTest._

  private val dataMetaStatus = data ~> meta("status") ~> narrowTo[JString]

  describe("with default (SimpleClassName) discriminator") {
    implicit val topEncoder: ResourceEncoder[Top] = deriveResourceEncoderForTrait[Top](meta("status"))

    it("should encode a direct-child leaf") {
      val doc = (DirectLeaf("A", 1): Top).toDocument.toJObject
      doc.asRootFocus(dataMetaStatus).shouldSucceed.value.value shouldBe "DirectLeaf"
    }

    it("should encode a leaf nested one level under an intermediate sealed trait") {
      val doc = (MidLeaf("B", "x"): Top).toDocument.toJObject
      doc.asRootFocus(dataMetaStatus).shouldSucceed.value.value shouldBe "MidLeaf"
    }

    it("should encode a leaf nested two levels deep") {
      val doc = (DeepestLeaf("C", true): Top).toDocument.toJObject
      doc.asRootFocus(dataMetaStatus).shouldSucceed.value.value shouldBe "DeepestLeaf"
    }
  }

  describe("with leaf-only CustomDiscriminator on a multi-level hierarchy") {
    // This is the original bug scenario: bateman 0.5.0 prior to encoder flattening would throw
    // `MissingDiscriminatorMapping[Mid]` at the `topEncoder` static initializer because the
    // factory queried the discriminator for each immediate subtype rather than each leaf.
    implicit val topEncoder: ResourceEncoder[Top] =
      deriveResourceEncoderForTrait[Top](
        meta("status"),
        CustomDiscriminator(
          forType[DirectLeaf]("direct"),
          forType[MidLeaf]("mid"),
          forType[DeepestLeaf]("deepest"),
        )
      )

    it("should encode a direct-child leaf") {
      val doc = (DirectLeaf("A", 1): Top).toDocument.toJObject
      doc.asRootFocus(dataMetaStatus).shouldSucceed.value.value shouldBe "direct"
    }

    it("should encode a leaf nested one level deep") {
      val doc = (MidLeaf("B", "x"): Top).toDocument.toJObject
      doc.asRootFocus(dataMetaStatus).shouldSucceed.value.value shouldBe "mid"
    }

    it("should encode a leaf nested two levels deep") {
      val doc = (DeepestLeaf("C", true): Top).toDocument.toJObject
      doc.asRootFocus(dataMetaStatus).shouldSucceed.value.value shouldBe "deepest"
    }
  }
}