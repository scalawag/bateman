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

package test.jsonapi.generic.decoding

import org.scalawag.bateman.json.{JObject, JObjectDecoder, UnexpectedValue}
import org.scalawag.bateman.json.generic.Config
import org.scalawag.bateman.json.literal._
import org.scalawag.bateman.json.lens.{focus, _}
import org.scalawag.bateman.jsonapi.generic.Annotations._
import org.scalawag.bateman.jsonapi.generic.semiauto._
import org.scalawag.bateman.jsonapi.lens._
import HListUnknownFieldsDecoderTest._

object HListUnknownFieldsDecoderTest {

  object TypeAndAttribute {
    implicit val config: Config = Config(allowUnknownFields = false)
    case class MyClass(@Type t: String, @Attribute a: Int)
    implicit val decoder: JObjectDecoder[MyClass] = deriveResourceDecoderForCaseClass[MyClass]()
  }

  object TypeAndAttributePermissive {
    case class MyClass(@Type t: String, @Attribute a: Int)
    implicit val decoder: JObjectDecoder[MyClass] = deriveResourceDecoderForCaseClass[MyClass]()
  }

  object TypeAndId {
    implicit val config: Config = Config(allowUnknownFields = false)
    case class MyClass(@Type t: String, @Id id: String)
    implicit val decoder: JObjectDecoder[MyClass] = deriveResourceDecoderForCaseClass[MyClass]()
  }

  object TypeIdAndAttribute {
    implicit val config: Config = Config(allowUnknownFields = false)
    case class MyClass(@Type t: String, @Id id: String, @Attribute a: Int)
    implicit val decoder: JObjectDecoder[MyClass] = deriveResourceDecoderForCaseClass[MyClass]()
  }

  object AttributeOnlyStrict {
    implicit val config: Config = Config(allowUnknownFields = false)
    case class MyClass(@Attribute a: Int)
    implicit val decoder: JObjectDecoder[MyClass] = deriveResourceDecoderForCaseClass[MyClass]()
  }

  object IdOnlyStrict {
    implicit val config: Config = Config(allowUnknownFields = false)
    case class MyClass(@Id id: String)
    implicit val decoder: JObjectDecoder[MyClass] = deriveResourceDecoderForCaseClass[MyClass]()
  }

  object TypeAndMeta {
    implicit val config: Config = Config(allowUnknownFields = false)
    case class MyClass(@Type t: String, @Meta m: Int)
    implicit val decoder: JObjectDecoder[MyClass] = deriveResourceDecoderForCaseClass[MyClass]()
  }

  object TypeAttributeAndMeta {
    implicit val config: Config = Config(allowUnknownFields = false)
    case class MyClass(@Type t: String, @Attribute a: Int, @Meta m: String)
    implicit val decoder: JObjectDecoder[MyClass] = deriveResourceDecoderForCaseClass[MyClass]()
  }
}

class HListUnknownFieldsDecoderTest extends HListDecoderTestBase {

  // --- allowUnknownFields = true bypasses all checks ---

  describe("permissive mode passes with extra fields everywhere") {
    import TypeAndAttributePermissive._
    val resource = Input(
      json"""{"type":"MyClass","attributes":{"a":42,"extra":true},"bogus":1}""",
      focus ~> narrowTo[JObject]
    )
    resource.succeedsWith(MyClass("MyClass", 42))
  }

  // --- whitelisted top-level fields ---

  describe("type field is whitelisted at the top level") {
    import AttributeOnlyStrict._
    val resource = Input(json"""{"type":"MyClass","attributes":{"a":42}}""", resourceType)
    resource.succeedsWith(MyClass(42))
  }

  describe("meta container is whitelisted at the top level") {
    import TypeAndAttribute._
    val resource = Input(json"""{"type":"MyClass","attributes":{"a":42},"meta":{}}""", focus ~> narrowTo[JObject])
    resource.succeedsWith(MyClass("MyClass", 42))
  }

  describe("relationships container is whitelisted at the top level") {
    import TypeAndAttribute._
    val resource = Input(json"""{"type":"MyClass","attributes":{"a":42},"relationships":{}}""", focus ~> narrowTo[JObject])
    resource.succeedsWith(MyClass("MyClass", 42))
  }

  describe("links container is whitelisted at the top level") {
    import TypeAndAttribute._
    val resource = Input(json"""{"type":"MyClass","attributes":{"a":42},"links":{"self":"http://example.com"}}""", focus ~> narrowTo[JObject])
    resource.succeedsWith(MyClass("MyClass", 42))
  }

  // --- consumed top-level fields ---

  describe("consumed id field is not flagged") {
    import TypeAndId._
    val resource = Input(json"""{"type":"MyClass","id":"123"}""", focus ~> narrowTo[JObject])
    resource.succeedsWith(MyClass("MyClass", "123"))
  }

  describe("unconsumed id field is flagged") {
    import TypeAndAttribute._
    val resource = Input(json"""{"type":"MyClass","id":"123","attributes":{"a":42}}""", "id")
    resource.failsWith[MyClass](UnexpectedValue(_))
  }

  // --- unknown top-level fields ---

  describe("unknown top-level field is flagged") {
    import TypeAndAttribute._
    val resource = Input(json"""{"type":"MyClass","attributes":{"a":42},"bogus":1}""", "bogus")
    resource.failsWith[MyClass](UnexpectedValue(_))
  }

  // --- unconsumed container fields ---

  describe("extra attribute field is flagged") {
    import TypeAndAttribute._
    val resource = Input(json"""{"type":"MyClass","attributes":{"a":42,"extra":true}}""", attributes ~> "extra")
    resource.failsWith[MyClass](UnexpectedValue(_))
  }

  describe("extra meta field is flagged") {
    import TypeAndMeta._
    val resource = Input(json"""{"type":"MyClass","meta":{"m":1,"extra":"x"}}""", meta ~> "extra")
    resource.failsWith[MyClass](UnexpectedValue(_))
  }

  // --- all consumed fields pass ---

  describe("all fields consumed across attributes and meta") {
    import TypeAttributeAndMeta._
    val resource = Input(
      json"""{"type":"MyClass","attributes":{"a":42},"meta":{"m":"hello"}}""",
      focus ~> narrowTo[JObject]
    )
    resource.succeedsWith(MyClass("MyClass", 42, "hello"))
  }

  describe("exact match with type and id") {
    import TypeIdAndAttribute._
    val resource = Input(json"""{"type":"MyClass","id":"123","attributes":{"a":42}}""", focus ~> narrowTo[JObject])
    resource.succeedsWith(MyClass("MyClass", "123", 42))
  }
}
