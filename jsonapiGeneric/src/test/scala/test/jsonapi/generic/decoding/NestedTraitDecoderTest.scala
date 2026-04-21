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

package test.jsonapi.generic.decoding

import org.scalawag.bateman.json.JObjectDecoder
import org.scalawag.bateman.json.literal._
import org.scalawag.bateman.jsonapi.generic.Annotations._
import org.scalawag.bateman.jsonapi.generic.semiauto._
import org.scalawag.bateman.jsonapi.lens._
import test.json.BatemanTestBase

object NestedTraitDecoderTest {
  // A multi-level sealed hierarchy where leaf types are nested under intermediate sealed traits.
  // The trait encoder writes the leaf's simple class name as the discriminator value (via overwrite
  // semantics in addDiscriminator), so the decoder must recognize leaves even when they are not
  // direct children of the top-level sealed trait.
  sealed trait Top
  final case class DirectLeaf(@Id id: String, @Attribute a: Int) extends Top

  sealed trait Mid extends Top
  final case class MidLeaf(@Id id: String, @Attribute b: String) extends Mid

  sealed trait Deeper extends Mid
  final case class DeepestLeaf(@Id id: String, @Attribute c: Boolean) extends Deeper

  implicit val directLeafDecoder: JObjectDecoder[DirectLeaf] =
    deriveResourceDecoderForCaseClass[DirectLeaf]("thing")
  implicit val midLeafDecoder: JObjectDecoder[MidLeaf] =
    deriveResourceDecoderForCaseClass[MidLeaf]("thing")
  implicit val deepestLeafDecoder: JObjectDecoder[DeepestLeaf] =
    deriveResourceDecoderForCaseClass[DeepestLeaf]("thing")

  implicit val topDecoder: JObjectDecoder[Top] =
    deriveResourceDecoderForTrait[Top](meta("status"))
}

class NestedTraitDecoderTest extends BatemanTestBase {
  import NestedTraitDecoderTest._

  it("should decode a direct-child leaf") {
    val doc = json"""
      {"data": {"type": "thing", "id": "A", "meta": {"status": "DirectLeaf"}, "attributes": {"a": 1}}}
    """
    doc.asRootFocus(data).shouldSucceed.decode[Top].shouldSucceed shouldBe DirectLeaf("A", 1)
  }

  it("should decode a leaf nested one level under an intermediate sealed trait") {
    val doc = json"""
      {"data": {"type": "thing", "id": "B", "meta": {"status": "MidLeaf"}, "attributes": {"b": "x"}}}
    """
    doc.asRootFocus(data).shouldSucceed.decode[Top].shouldSucceed shouldBe MidLeaf("B", "x")
  }

  it("should decode a leaf nested two levels deep") {
    val doc = json"""
      {"data": {"type": "thing", "id": "C", "meta": {"status": "DeepestLeaf"}, "attributes": {"c": true}}}
    """
    doc.asRootFocus(data).shouldSucceed.decode[Top].shouldSucceed shouldBe DeepestLeaf("C", true)
  }
}