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

package test.json.generic.decoding

import org.scalawag.bateman.json.JObjectDecoder
import org.scalawag.bateman.json.literal._
import org.scalawag.bateman.json.generic.semiauto._
import test.json.BatemanTestBase

object NestedTraitDecoderTest {
  // A multi-level sealed hierarchy where leaf types are nested under intermediate sealed traits.
  // Trait encoders write the leaf type's simple class name as the discriminator value (via
  // overwrite semantics in addDiscriminator), so the decoder must recognize leaves even when
  // they are not direct children of the top-level sealed trait.
  sealed trait Top
  final case class DirectLeaf(a: Int) extends Top

  sealed trait Mid extends Top
  final case class MidLeaf(b: String) extends Mid

  sealed trait Deeper extends Mid
  final case class DeepestLeaf(c: Boolean) extends Deeper
}

class NestedTraitDecoderTest extends BatemanTestBase {
  import NestedTraitDecoderTest._
  import org.scalawag.bateman.json.generic.auto._

  implicit val topDecoder: JObjectDecoder[Top] = deriveDecoderForTrait[Top]()

  it("should decode a direct-child leaf") {
    json"""{"type":"DirectLeaf","a":1}""".asRootFocus.decode[Top].shouldSucceed shouldBe DirectLeaf(1)
  }

  it("should decode a leaf nested one level under an intermediate sealed trait") {
    json"""{"type":"MidLeaf","b":"x"}""".asRootFocus.decode[Top].shouldSucceed shouldBe MidLeaf("x")
  }

  it("should decode a leaf nested two levels deep") {
    json"""{"type":"DeepestLeaf","c":true}""".asRootFocus.decode[Top].shouldSucceed shouldBe DeepestLeaf(true)
  }
}