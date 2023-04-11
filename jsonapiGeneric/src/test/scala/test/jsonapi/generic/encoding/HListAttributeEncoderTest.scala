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

import org.scalawag.bateman.jsonapi.generic.Annotations.Attribute
import org.scalawag.bateman.json.literal._
import org.scalawag.bateman.json.syntax._
import test.json.BatemanTestBase
import HListAttributeEncoderTest._
import org.scalawag.bateman.json.generic.Config
import org.scalawag.bateman.json.generic.naming.{CamelCase, PascalCase}
import org.scalawag.bateman.json.{NotNull, Null, Nullable}

object HListAttributeEncoderTest {
  object MyIdAttribute {
    case class MyClass(@Attribute a: Int)
  }
  object MyNullableAttribute {
    case class MyClass(@Attribute a: Nullable[Int])
  }
  object MyOptionIdAttribute {
    case class MyClass(@Attribute a: Option[Int])
  }
  object MyOptionNullableAttribute {
    case class MyClass(@Attribute a: Option[Nullable[Int]])
  }
  object MyIdAttributeWithDefault {
    case class MyClass(@Attribute a: Int = 417)
  }
  object MyNullableAttributeWithDefault {
    case class MyClass(@Attribute a: Nullable[Int] = NotNull(5))
  }
  object MyOptionIdAttributeWithDefault {
    case class MyClass(@Attribute a: Option[Int] = Some(7))
  }
  object MyOptionNullableAttributeWithDefault {
    case class MyClass(@Attribute a: Option[Nullable[Int]] = Some(NotNull(23)))
  }
}

class HListAttributeEncoderTest extends BatemanTestBase {
  describe("MyIdAttribute") {
    import MyIdAttribute._
    import org.scalawag.bateman.jsonapi.generic.auto._

    it("should encode") {
      MyClass(17).toJAny shouldEncodeTo json"""{"type": "MyClass", "attributes": {"a": 17}}"""
    }
  }

  describe("MyNullableAttribute") {
    import MyNullableAttribute._
    import org.scalawag.bateman.jsonapi.generic.auto._

    it("should encode NotNull") {
      MyClass(NotNull(17)).toJAny shouldEncodeTo json"""{"type": "MyClass", "attributes": {"a": 17}}"""
    }

    it("should encode Null") {
      MyClass(Null).toJAny shouldEncodeTo json"""{"type": "MyClass", "attributes": {"a": null}}"""
    }
  }

  describe("MyOptionIdAttribute") {
    import MyOptionIdAttribute._
    import org.scalawag.bateman.jsonapi.generic.auto._

    it("should encode Some") {
      MyClass(Some(17)).toJAny shouldEncodeTo json"""{"type": "MyClass", "attributes": {"a": 17}}"""
    }

    it("should encode None") {
      MyClass(None).toJAny shouldEncodeTo json"""{"type": "MyClass"}"""
    }
  }

  describe("MyOptionNullableAttribute") {
    import MyOptionNullableAttribute._
    import org.scalawag.bateman.jsonapi.generic.auto._

    it("should encode Some[NotNull]") {
      MyClass(Some(NotNull(17))).toJAny shouldEncodeTo json"""{"type": "MyClass", "attributes": {"a": 17}}"""
    }

    it("should encode Some[Null]") {
      MyClass(Some(Null)).toJAny shouldEncodeTo json"""{"type": "MyClass", "attributes": {"a": null}}"""
    }

    it("should encode None") {
      MyClass(None).toJAny shouldEncodeTo json"""{"type": "MyClass"}"""
    }
  }

  describe("MyIdAttributeWithDefault") {
    import MyIdAttributeWithDefault._
    import org.scalawag.bateman.jsonapi.generic.auto._

    it("should encode") {
      MyClass(17).toJAny shouldEncodeTo json"""{"type": "MyClass", "attributes": {"a": 17}}"""
    }

    it("should not encode default") {
      MyClass(417).toJAny shouldEncodeTo json"""{"type": "MyClass"}"""
    }
  }

  describe("MyNullableAttributeWithDefault") {
    import MyNullableAttributeWithDefault._
    import org.scalawag.bateman.jsonapi.generic.auto._

    it("should encode NotNull") {
      MyClass(NotNull(17)).toJAny shouldEncodeTo json"""{"type": "MyClass", "attributes": {"a": 17}}"""
    }

    it("should encode Null") {
      MyClass(Null).toJAny shouldEncodeTo json"""{"type": "MyClass", "attributes": {"a": null}}"""
    }

    it("should not encode default") {
      MyClass(NotNull(5)).toJAny shouldEncodeTo json"""{"type": "MyClass"}"""
    }
  }

  describe("MyOptionIdAttributeWithDefault") {
    import MyOptionIdAttributeWithDefault._
    import org.scalawag.bateman.jsonapi.generic.auto._

    it("should encode Some") {
      MyClass(Some(17)).toJAny shouldEncodeTo json"""{"type": "MyClass", "attributes": {"a": 17}}"""
    }

    it("should encode None") {
      MyClass(None).toJAny shouldEncodeTo json"""{"type": "MyClass"}"""
    }

    it("should not encode default") {
      MyClass(Some(7)).toJAny shouldEncodeTo json"""{"type": "MyClass"}"""
    }
  }

  describe("MyOptionNullableAttributeWithDefault") {
    import MyOptionNullableAttributeWithDefault._
    import org.scalawag.bateman.jsonapi.generic.auto._

    it("should encode Some[NotNull]") {
      MyClass(Some(NotNull(17))).toJAny shouldEncodeTo json"""{"type": "MyClass", "attributes": {"a": 17}}"""
    }

    it("should encode Some[Null]") {
      MyClass(Some(Null)).toJAny shouldEncodeTo json"""{"type": "MyClass", "attributes": {"a": null}}"""
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
      import MyOptionIdAttributeWithDefault._
      import org.scalawag.bateman.jsonapi.generic.auto._
      implicit val config: Config = Config(encodeDefaultValues = true)

      MyClass(Some(7)).toJAny shouldEncodeTo json"""{"type": "MyClass", "attributes": {"a": 7}}"""
    }

    it("should use the field name mapper") {
      import MyOptionIdAttributeWithDefault._
      import org.scalawag.bateman.jsonapi.generic.auto._
      implicit val config: Config = Config(fieldNameMapping = CamelCase to PascalCase)

      MyClass(Some(17)).toJAny shouldEncodeTo json"""{"type": "MyClass", "attributes": {"A": 17}}"""
    }
  }
}
