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

package test.json.generic.encoding

import org.scalawag.bateman.json.JObjectEncoder
import org.scalawag.bateman.json.generic.Discriminators._
import org.scalawag.bateman.json.generic.semiauto._
import org.scalawag.bateman.json.lens._
import org.scalawag.bateman.json.literal._
import test.json.BatemanTestBase

object NestedTraitEncoderTest {
  // A multi-level sealed hierarchy where leaf types are nested under intermediate sealed traits.
  // Trait encoder dispatch flattens the hierarchy at derivation time, so leaf-only discriminator
  // mappings work — callers do not need to provide layered mappings for intermediate sealed traits.
  sealed trait Top
  final case class DirectLeaf(a: Int) extends Top

  sealed trait Mid extends Top
  final case class MidLeaf(b: String) extends Mid

  sealed trait Deeper extends Mid
  final case class DeepestLeaf(c: Boolean) extends Deeper

  // Diamond: a concrete type that extends multiple sealed traits, each a (transitive) child of Top.
  sealed trait LeftBranch extends Top
  sealed trait RightBranch extends Top
  final case class Diamond(d: Int) extends LeftBranch with RightBranch

  implicit val directLeafEncoder: JObjectEncoder[DirectLeaf] = deriveEncoderForCaseClass[DirectLeaf]()
  implicit val midLeafEncoder: JObjectEncoder[MidLeaf] = deriveEncoderForCaseClass[MidLeaf]()
  implicit val deepestLeafEncoder: JObjectEncoder[DeepestLeaf] = deriveEncoderForCaseClass[DeepestLeaf]()
  implicit val diamondEncoder: JObjectEncoder[Diamond] = deriveEncoderForCaseClass[Diamond]()
}

class NestedTraitEncoderTest extends BatemanTestBase {
  import NestedTraitEncoderTest._

  describe("with default (SimpleClassName) discriminator") {
    implicit val topEncoder: JObjectEncoder[Top] = deriveEncoderForTrait[Top]()

    it("should encode a direct-child leaf") {
      val v: Top = DirectLeaf(1)
      v shouldEncodeTo json"""{"type":"DirectLeaf","a":1}"""
    }

    it("should encode a leaf nested one level under an intermediate sealed trait") {
      val v: Top = MidLeaf("x")
      v shouldEncodeTo json"""{"type":"MidLeaf","b":"x"}"""
    }

    it("should encode a leaf nested two levels deep") {
      val v: Top = DeepestLeaf(true)
      v shouldEncodeTo json"""{"type":"DeepestLeaf","c":true}"""
    }

    it("should encode a leaf reachable via multiple sealed trait branches (diamond)") {
      val v: Top = Diamond(7)
      v shouldEncodeTo json"""{"type":"Diamond","d":7}"""
    }
  }

  describe("with leaf-only CustomDiscriminator on a multi-level hierarchy") {
    // This is the case that previously failed with `MissingDiscriminatorMapping[Mid]` (or similar
    // for the intermediate trait) because the encoder factory queried the discriminator about each
    // immediate subtype rather than each transitive leaf.
    implicit val topEncoder: JObjectEncoder[Top] =
      deriveEncoderForTrait[Top](
        "type",
        CustomDiscriminator(
          forType[DirectLeaf]("direct"),
          forType[MidLeaf]("mid"),
          forType[DeepestLeaf]("deepest"),
          forType[Diamond]("diamond"),
        )
      )

    it("should encode a direct-child leaf") {
      val v: Top = DirectLeaf(1)
      v shouldEncodeTo json"""{"type":"direct","a":1}"""
    }

    it("should encode a leaf nested one level deep") {
      val v: Top = MidLeaf("x")
      v shouldEncodeTo json"""{"type":"mid","b":"x"}"""
    }

    it("should encode a leaf nested two levels deep") {
      val v: Top = DeepestLeaf(true)
      v shouldEncodeTo json"""{"type":"deepest","c":true}"""
    }

    it("should encode a diamond leaf") {
      val v: Top = Diamond(7)
      v shouldEncodeTo json"""{"type":"diamond","d":7}"""
    }
  }

  describe("with layered CustomDiscriminator (forType on an intermediate trait)") {
    // Verifies that layered routing still works after flattening: every leaf under `Mid` matches
    // `forType[Mid]` via isAssignableFrom and is routed through the intermediate `midEncoder`,
    // which writes its own discriminator value before dispatching to the leaf.
    implicit val midEncoder: JObjectEncoder[Mid] =
      deriveEncoderForTrait[Mid](
        "subtype",
        CustomDiscriminator(
          forType[MidLeaf]("mid_leaf"),
          forType[DeepestLeaf]("deepest_leaf"),
        )
      )

    implicit val topEncoder: JObjectEncoder[Top] =
      deriveEncoderForTrait[Top](
        "type",
        CustomDiscriminator(
          forType[DirectLeaf]("direct"),
          forType[Mid]("mid_branch"),
          forType[Diamond]("diamond"),
        )
      )

    it("should encode a direct-child leaf without invoking the intermediate") {
      val v: Top = DirectLeaf(1)
      v shouldEncodeTo json"""{"type":"direct","a":1}"""
    }

    it("should encode a mid-branch leaf via the intermediate trait's encoder") {
      val v: Top = MidLeaf("x")
      v shouldEncodeTo json"""{"type":"mid_branch","subtype":"mid_leaf","b":"x"}"""
    }

    it("should encode a deeper-branch leaf via the intermediate trait's encoder") {
      val v: Top = DeepestLeaf(true)
      v shouldEncodeTo json"""{"type":"mid_branch","subtype":"deepest_leaf","c":true}"""
    }
  }
}