// bateman -- Copyright 2021-2023 -- Justin Patterson
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

import org.scalawag.bateman.jsonapi.generic.Annotations.Meta
import org.scalawag.bateman.json.literal._
import org.scalawag.bateman.json.syntax._
import test.json.BatemanTestBase
import HListMetaEncoderTest._
import org.scalawag.bateman.json.generic.Config
import org.scalawag.bateman.json.generic.naming.{CamelCase, PascalCase}
import org.scalawag.bateman.json.{NotNull, Null, Nullable}

object HListMetaEncoderTest {
  object MyIdMeta {
    case class MyClass(@Meta a: Int)
  }
  object MyNullableMeta {
    case class MyClass(@Meta a: Nullable[Int])
  }
  object MyOptionIdMeta {
    case class MyClass(@Meta a: Option[Int])
  }
  object MyOptionNullableMeta {
    case class MyClass(@Meta a: Option[Nullable[Int]])
  }
  object MyIdMetaWithDefault {
    case class MyClass(@Meta a: Int = 417)
  }
  object MyNullableMetaWithDefault {
    case class MyClass(@Meta a: Nullable[Int] = NotNull(5))
  }
  object MyOptionIdMetaWithDefault {
    case class MyClass(@Meta a: Option[Int] = Some(7))
  }
  object MyOptionNullableMetaWithDefault {
    case class MyClass(@Meta a: Option[Nullable[Int]] = Some(NotNull(23)))
  }
}

class HListMetaEncoderTest extends BatemanTestBase {
  describe("MyIdMeta") {
    import MyIdMeta._
    import org.scalawag.bateman.jsonapi.generic.auto._

    it("should encode") {
      MyClass(17).toJAny shouldEncodeTo json"""{"type": "MyClass", "meta": {"a": 17}}"""
    }
  }

  describe("MyNullableMeta") {
    import MyNullableMeta._
    import org.scalawag.bateman.jsonapi.generic.auto._

    it("should encode NotNull") {
      MyClass(NotNull(17)).toJAny shouldEncodeTo json"""{"type": "MyClass", "meta": {"a": 17}}"""
    }

    it("should encode Null") {
      MyClass(Null).toJAny shouldEncodeTo json"""{"type": "MyClass", "meta": {"a": null}}"""
    }
  }

  describe("MyOptionIdMeta") {
    import MyOptionIdMeta._
    import org.scalawag.bateman.jsonapi.generic.auto._

    it("should encode Some") {
      MyClass(Some(17)).toJAny shouldEncodeTo json"""{"type": "MyClass", "meta": {"a": 17}}"""
    }

    it("should encode None") {
      MyClass(None).toJAny shouldEncodeTo json"""{"type": "MyClass"}"""
    }
  }

  describe("MyOptionNullableMeta") {
    import MyOptionNullableMeta._
    import org.scalawag.bateman.jsonapi.generic.auto._

    it("should encode Some[NotNull]") {
      MyClass(Some(NotNull(17))).toJAny shouldEncodeTo json"""{"type": "MyClass", "meta": {"a": 17}}"""
    }

    it("should encode Some[Null]") {
      MyClass(Some(Null)).toJAny shouldEncodeTo json"""{"type": "MyClass", "meta": {"a": null}}"""
    }

    it("should encode None") {
      MyClass(None).toJAny shouldEncodeTo json"""{"type": "MyClass"}"""
    }
  }

  describe("MyIdMetaWithDefault") {
    import MyIdMetaWithDefault._
    import org.scalawag.bateman.jsonapi.generic.auto._

    it("should encode") {
      MyClass(17).toJAny shouldEncodeTo json"""{"type": "MyClass", "meta": {"a": 17}}"""
    }

    it("should not encode default") {
      MyClass(417).toJAny shouldEncodeTo json"""{"type": "MyClass"}"""
    }
  }

  describe("MyNullableMetaWithDefault") {
    import MyNullableMetaWithDefault._
    import org.scalawag.bateman.jsonapi.generic.auto._

    it("should encode NotNull") {
      MyClass(NotNull(17)).toJAny shouldEncodeTo json"""{"type": "MyClass", "meta": {"a": 17}}"""
    }

    it("should encode Null") {
      MyClass(Null).toJAny shouldEncodeTo json"""{"type": "MyClass", "meta": {"a": null}}"""
    }

    it("should not encode default") {
      MyClass(NotNull(5)).toJAny shouldEncodeTo json"""{"type": "MyClass"}"""
    }
  }

  describe("MyOptionIdMetaWithDefault") {
    import MyOptionIdMetaWithDefault._
    import org.scalawag.bateman.jsonapi.generic.auto._

    it("should encode Some") {
      MyClass(Some(17)).toJAny shouldEncodeTo json"""{"type": "MyClass", "meta": {"a": 17}}"""
    }

    it("should encode None") {
      MyClass(None).toJAny shouldEncodeTo json"""{"type": "MyClass"}"""
    }

    it("should not encode default") {
      MyClass(Some(7)).toJAny shouldEncodeTo json"""{"type": "MyClass"}"""
    }
  }

  describe("MyOptionNullableMetaWithDefault") {
    import MyOptionNullableMetaWithDefault._
    import org.scalawag.bateman.jsonapi.generic.auto._

    it("should encode Some[NotNull]") {
      MyClass(Some(NotNull(17))).toJAny shouldEncodeTo json"""{"type": "MyClass", "meta": {"a": 17}}"""
    }

    it("should encode Some[Null]") {
      MyClass(Some(Null)).toJAny shouldEncodeTo json"""{"type": "MyClass", "meta": {"a": null}}"""
    }

    it("should encode None") {
      MyClass(None).toJAny shouldEncodeTo json"""{"type": "MyClass"}"""
    }

    it("should not encode default") {
      MyClass(Some(NotNull(23))).toJAny shouldEncodeTo json"""{"type": "MyClass"}"""
    }
  }


  describe("config") {
    it("should encode default value when so configured") {
      import MyOptionIdMetaWithDefault._
      import org.scalawag.bateman.jsonapi.generic.auto._
      implicit val config: Config = Config(encodeDefaultValues = true)

      MyClass(Some(7)).toJAny shouldEncodeTo json"""{"type": "MyClass", "meta": {"a": 7}}"""
    }

    it("should use the field name mapper") {
      import MyOptionIdMetaWithDefault._
      import org.scalawag.bateman.jsonapi.generic.auto._
      implicit val config: Config = Config(fieldNameMapping = CamelCase to PascalCase)

      MyClass(Some(17)).toJAny shouldEncodeTo json"""{"type": "MyClass", "meta": {"A": 17}}"""
    }
  }
}
