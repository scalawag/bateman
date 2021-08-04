// bateman -- Copyright 2021 -- Justin Patterson
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

package org.scalawag.bateman.jsonapi.generic.encoding

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import org.scalawag.bateman.json.syntax._
import org.scalawag.bateman.json.{DataDrivenTestUtils, NotNull, Null, Nullable, ParserTestUtils}
import OptionTest._
import org.scalawag.bateman.json.encoding.{JNull, JNumber, JStringEncoder}
import org.scalawag.bateman.json.generic.Config
import org.scalawag.bateman.jsonapi.encoding.{NullData, Relationship, ResourceIdentifier, ResourceObjectOptionalId}
import org.scalawag.bateman.jsonapi.generic.{AttributeTag, IdTag, MetaTag, RelationshipTag, semiauto}
import shapeless.tag.@@

object OptionTest {
  final case class MyIdClass(id: String @@ IdTag)
  final case class MyIdClassDefault(id: String @@ IdTag = "1")
  final case class MyIdClassOption(id: Option[String] @@ IdTag)
  final case class MyIdClassOptionDefault(id: Option[String] @@ IdTag = None)
  final case class MyIdClassOptionDefaultSome(id: Option[String] @@ IdTag = Some("1"))

  final case class MyAttrClass(
      a: Option[Int] @@ AttributeTag,
      b: Option[Int] @@ AttributeTag = None,
      c: Option[Int] @@ AttributeTag = Some(7),
      d: Int @@ AttributeTag,
      e: Int @@ AttributeTag = 7,
      f: Nullable[Int] @@ AttributeTag,
      g: Nullable[Int] @@ AttributeTag = Null,
      h: Nullable[Int] @@ AttributeTag = NotNull(7),
      i: Option[Nullable[Int]] @@ AttributeTag,
      j: Option[Nullable[Int]] @@ AttributeTag = None,
      k: Option[Nullable[Int]] @@ AttributeTag = Some(Null),
      l: Option[Nullable[Int]] @@ AttributeTag = Some(NotNull(7))
  )

  object MyAttrClass {
    val minimal = MyAttrClass(a = None, d = 7, f = Null, i = None)

    val full = MyAttrClass(
      a = Some(1),
      b = Some(2),
      c = Some(3),
      d = 4,
      e = 5,
      f = NotNull(6),
      g = NotNull(7),
      h = NotNull(8),
      i = Some(NotNull(9)),
      j = Some(NotNull(10)),
      k = Some(NotNull(11)),
      l = Some(NotNull(12)),
    )
  }

  final case class MyMetaClass(
      a: Option[Int] @@ MetaTag,
      b: Option[Int] @@ MetaTag = None,
      c: Option[Int] @@ MetaTag = Some(7),
      d: Int @@ MetaTag,
      e: Int @@ MetaTag = 7,
      f: Nullable[Int] @@ MetaTag,
      g: Nullable[Int] @@ MetaTag = Null,
      h: Nullable[Int] @@ MetaTag = NotNull(7),
      i: Option[Nullable[Int]] @@ MetaTag,
      j: Option[Nullable[Int]] @@ MetaTag = None,
      k: Option[Nullable[Int]] @@ MetaTag = Some(Null),
      l: Option[Nullable[Int]] @@ MetaTag = Some(NotNull(7))
  )

  object MyMetaClass {
    val minimal = MyMetaClass(a = None, d = 7, f = Null, i = None)

    val full = MyMetaClass(
      a = Some(1),
      b = Some(2),
      c = Some(3),
      d = 4,
      e = 5,
      f = NotNull(6),
      g = NotNull(7),
      h = NotNull(8),
      i = Some(NotNull(9)),
      j = Some(NotNull(10)),
      k = Some(NotNull(11)),
      l = Some(NotNull(12)),
    )
  }

  final case class MyRelativeId(id: Int @@ IdTag)

  final case class MyRelClass(
      a: Option[MyRelativeId] @@ RelationshipTag,
      b: Option[MyRelativeId] @@ RelationshipTag = None,
      c: Option[MyRelativeId] @@ RelationshipTag = Some(MyRelativeId(7)),
      d: MyRelativeId @@ RelationshipTag,
      e: MyRelativeId @@ RelationshipTag = MyRelativeId(7),
      f: Nullable[MyRelativeId] @@ RelationshipTag,
      g: Nullable[MyRelativeId] @@ RelationshipTag = Null,
      h: Nullable[MyRelativeId] @@ RelationshipTag = NotNull(MyRelativeId(7)),
      i: Option[Nullable[MyRelativeId]] @@ RelationshipTag,
      j: Option[Nullable[MyRelativeId]] @@ RelationshipTag = None,
      k: Option[Nullable[MyRelativeId]] @@ RelationshipTag = Some(Null),
      l: Option[Nullable[MyRelativeId]] @@ RelationshipTag = Some(NotNull(MyRelativeId(7))),
      m: List[MyRelativeId] @@ RelationshipTag,
      n: List[MyRelativeId] @@ RelationshipTag = Nil,
      o: List[MyRelativeId] @@ RelationshipTag = List(MyRelativeId(7), MyRelativeId(8)),
      p: Option[List[MyRelativeId]] @@ RelationshipTag,
      q: Option[List[MyRelativeId]] @@ RelationshipTag = None,
      r: Option[List[MyRelativeId]] @@ RelationshipTag = Some(Nil)
  )

  object MyRelClass {
    val minimal = MyRelClass(a = None, d = MyRelativeId(7), f = Null, i = None, m = Nil, p = None)

    val full = MyRelClass(
      a = Some(MyRelativeId(1)),
      b = Some(MyRelativeId(2)),
      c = Some(MyRelativeId(3)),
      d = MyRelativeId(4),
      e = MyRelativeId(5),
      f = NotNull(MyRelativeId(6)),
      g = NotNull(MyRelativeId(7)),
      h = NotNull(MyRelativeId(8)),
      i = Some(NotNull(MyRelativeId(9))),
      j = Some(NotNull(MyRelativeId(10))),
      k = Some(NotNull(MyRelativeId(11))),
      l = Some(NotNull(MyRelativeId(12))),
      m = List(MyRelativeId(13), MyRelativeId(14)),
      n = List(MyRelativeId(15), MyRelativeId(16)),
      o = List(MyRelativeId(17), MyRelativeId(18)),
      p = Some(List(MyRelativeId(19), MyRelativeId(20))),
      q = Some(List(MyRelativeId(21), MyRelativeId(22))),
      r = Some(List(MyRelativeId(23), MyRelativeId(24))),
    )
  }

}

class OptionTest extends AnyFunSpec with Matchers with ParserTestUtils with DataDrivenTestUtils {

  describe("id") {
    it("should handle required ID with no default") {
      implicit val enc = semiauto.deriveResourceObjectOptionalIdEncoderForCaseClass[MyIdClass]()

      MyIdClass("1").to[ResourceObjectOptionalId] shouldBe ResourceObjectOptionalId(
        "MyIdClass",
        Some("1")
      )
    }

    it("should handle required ID with default (even when set to not encode defaults)") {
      implicit val enc = semiauto.deriveResourceObjectOptionalIdEncoderForCaseClass[MyIdClassDefault]()

      MyIdClassDefault().to[ResourceObjectOptionalId] shouldBe ResourceObjectOptionalId(
        "MyIdClassDefault",
        Some("1")
      )
    }

    it("should handle option ID without default") {
      implicit val enc = semiauto.deriveResourceObjectOptionalIdEncoderForCaseClass[MyIdClassOption]()

      MyIdClassOption(None).to[ResourceObjectOptionalId] shouldBe ResourceObjectOptionalId(
        "MyIdClassOption"
      )
    }

    it("should handle option ID with default None") {
      implicit val enc = semiauto.deriveResourceObjectOptionalIdEncoderForCaseClass[MyIdClassOptionDefault]()

      MyIdClassOptionDefault(None).to[ResourceObjectOptionalId] shouldBe ResourceObjectOptionalId(
        "MyIdClassOptionDefault"
      )
    }

    it("should handle option ID with default None and value Some") {
      implicit val enc = semiauto.deriveResourceObjectOptionalIdEncoderForCaseClass[MyIdClassOptionDefault]()

      MyIdClassOptionDefault(Some("1")).to[ResourceObjectOptionalId] shouldBe ResourceObjectOptionalId(
        "MyIdClassOptionDefault",
        Some("1")
      )
    }

    it("should handle option ID with default Some (even when set to not encode defaults)") {
      implicit val enc = semiauto.deriveResourceObjectOptionalIdEncoderForCaseClass[MyIdClassOptionDefaultSome]()

      MyIdClassOptionDefaultSome().to[ResourceObjectOptionalId] shouldBe ResourceObjectOptionalId(
        "MyIdClassOptionDefaultSome",
        Some("1")
      )
    }
  }

  describe("attributes") {
    it("should succeed without defaults") {
      implicit val enc = semiauto.deriveResourceObjectOptionalIdEncoderForCaseClass[MyAttrClass]()

      MyAttrClass.minimal.to[ResourceObjectOptionalId] shouldBe ResourceObjectOptionalId(
        "MyAttrClass",
        attributes = Some(
          Map(
            "d" -> JNumber(7),
            "f" -> JNull,
          )
        )
      )
    }

    it("should succeed with defaults") {
      implicit val enc = semiauto.deriveResourceObjectOptionalIdEncoderForCaseClass[MyAttrClass](config =
        Config.default.copy(encodeDefaultValues = true)
      )

      MyAttrClass.minimal.to[ResourceObjectOptionalId] shouldBe ResourceObjectOptionalId(
        "MyAttrClass",
        attributes = Some(
          Map(
            "c" -> JNumber(7),
            "d" -> JNumber(7),
            "e" -> JNumber(7),
            "f" -> JNull,
            "g" -> JNull,
            "h" -> JNumber(7),
            "k" -> JNull,
            "l" -> JNumber(7),
          )
        )
      )
    }

    it("should succeed full instance") {
      implicit val enc = semiauto.deriveResourceObjectOptionalIdEncoderForCaseClass[MyAttrClass]()

      MyAttrClass.full.to[ResourceObjectOptionalId] shouldBe ResourceObjectOptionalId(
        "MyAttrClass",
        attributes = Some(
          Map(
            "a" -> JNumber(1),
            "b" -> JNumber(2),
            "c" -> JNumber(3),
            "d" -> JNumber(4),
            "e" -> JNumber(5),
            "f" -> JNumber(6),
            "g" -> JNumber(7),
            "h" -> JNumber(8),
            "i" -> JNumber(9),
            "j" -> JNumber(10),
            "k" -> JNumber(11),
            "l" -> JNumber(12),
          )
        )
      )
    }
  }

  describe("meta") {
    it("should succeed without defaults") {
      implicit val enc = semiauto.deriveResourceObjectOptionalIdEncoderForCaseClass[MyMetaClass]()

      MyMetaClass.minimal.to[ResourceObjectOptionalId] shouldBe ResourceObjectOptionalId(
        "MyMetaClass",
        meta = Some(
          Map(
            "d" -> JNumber(7),
            "f" -> JNull,
          )
        )
      )
    }

    it("should succeed with defaults") {
      implicit val enc = semiauto.deriveResourceObjectOptionalIdEncoderForCaseClass[MyMetaClass](config =
        Config.default.copy(encodeDefaultValues = true)
      )

      MyMetaClass.minimal.to[ResourceObjectOptionalId] shouldBe ResourceObjectOptionalId(
        "MyMetaClass",
        meta = Some(
          Map(
            "c" -> JNumber(7),
            "d" -> JNumber(7),
            "e" -> JNumber(7),
            "f" -> JNull,
            "g" -> JNull,
            "h" -> JNumber(7),
            "k" -> JNull,
            "l" -> JNumber(7),
          )
        )
      )
    }

    it("should succeed full instance") {
      implicit val enc = semiauto.deriveResourceObjectOptionalIdEncoderForCaseClass[MyMetaClass]()

      MyMetaClass.full.to[ResourceObjectOptionalId] shouldBe ResourceObjectOptionalId(
        "MyMetaClass",
        meta = Some(
          Map(
            "a" -> JNumber(1),
            "b" -> JNumber(2),
            "c" -> JNumber(3),
            "d" -> JNumber(4),
            "e" -> JNumber(5),
            "f" -> JNumber(6),
            "g" -> JNumber(7),
            "h" -> JNumber(8),
            "i" -> JNumber(9),
            "j" -> JNumber(10),
            "k" -> JNumber(11),
            "l" -> JNumber(12),
          )
        )
      )
    }
  }

  describe("relationships") {
    implicit val intenc = JStringEncoder[String].contramap[Int](_.toString)
    implicit val idenc = semiauto.deriveResourceIdentifierEncoderForCaseClass[MyRelativeId]()

    def relToOne(id: Int): Relationship = Relationship(Some(ResourceIdentifier("MyRelativeId", id.toString)))
    def relToMany(ids: Int*): Relationship =
      Relationship(Some(ids.toList.map(i => ResourceIdentifier("MyRelativeId", i.toString))))
    val relNull: Relationship = Relationship(Some(NullData))

    it("should succeed without defaults") {
      implicit val enc = semiauto.deriveResourceObjectOptionalIdEncoderForCaseClass[MyRelClass]()

      MyRelClass.minimal.to[ResourceObjectOptionalId] shouldBe ResourceObjectOptionalId(
        "MyRelClass",
        relationships = Some(
          Map(
            "d" -> relToOne(7),
            "f" -> relNull,
            "m" -> relToMany()
          )
        )
      )
    }

    it("should succeed with defaults") {
      implicit val enc = semiauto.deriveResourceObjectOptionalIdEncoderForCaseClass[MyRelClass](config =
        Config.default.copy(encodeDefaultValues = true)
      )

      MyRelClass.minimal.to[ResourceObjectOptionalId] shouldBe ResourceObjectOptionalId(
        "MyRelClass",
        relationships = Some(
          Map(
            "c" -> relToOne(7),
            "d" -> relToOne(7),
            "e" -> relToOne(7),
            "f" -> relNull,
            "g" -> relNull,
            "h" -> relToOne(7),
            "k" -> relNull,
            "l" -> relToOne(7),
            "m" -> relToMany(),
            "n" -> relToMany(),
            "o" -> relToMany(7, 8),
            "r" -> relToMany(),
          )
        )
      )
    }

    it("should succeed full instance") {
      implicit val enc = semiauto.deriveResourceObjectOptionalIdEncoderForCaseClass[MyRelClass]()

      MyRelClass.full.to[ResourceObjectOptionalId] shouldBe ResourceObjectOptionalId(
        "MyRelClass",
        relationships = Some(
          Map(
            "a" -> relToOne(1),
            "b" -> relToOne(2),
            "c" -> relToOne(3),
            "d" -> relToOne(4),
            "e" -> relToOne(5),
            "f" -> relToOne(6),
            "g" -> relToOne(7),
            "h" -> relToOne(8),
            "i" -> relToOne(9),
            "j" -> relToOne(10),
            "k" -> relToOne(11),
            "l" -> relToOne(12),
            "m" -> relToMany(13, 14),
            "n" -> relToMany(15, 16),
            "o" -> relToMany(17, 18),
            "p" -> relToMany(19, 20),
            "q" -> relToMany(21, 22),
            "r" -> relToMany(23, 24),
          )
        )
      )
    }
  }
}
