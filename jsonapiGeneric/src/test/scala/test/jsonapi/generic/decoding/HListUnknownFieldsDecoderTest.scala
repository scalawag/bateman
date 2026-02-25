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
import org.scalawag.bateman.json.focus.weak._
import org.scalawag.bateman.jsonapi.generic.Annotations._
import org.scalawag.bateman.jsonapi.generic.semiauto.unchecked._
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
}

class HListUnknownFieldsDecoderTest extends HListDecoderTestBase {

  describe("@Type + @Attribute with allowUnknownFields = false") {
    import TypeAndAttribute._
    val resource = Input(json"""{"type":"MyClass","attributes":{"a":42}}""", focus ~> narrow[JObject])
    resource.succeedsWith(MyClass("MyClass", 42))
  }

  describe("@Type + @Attribute with allowUnknownFields = true") {
    import TypeAndAttributePermissive._
    val resource = Input(json"""{"type":"MyClass","attributes":{"a":42}}""", focus ~> narrow[JObject])
    resource.succeedsWith(MyClass("MyClass", 42))
  }

  describe("@Type + @Id with allowUnknownFields = false") {
    import TypeAndId._
    val resource = Input(json"""{"type":"MyClass","id":"123"}""", focus ~> narrow[JObject])
    resource.succeedsWith(MyClass("MyClass", "123"))
  }

  describe("@Type + @Id + @Attribute with allowUnknownFields = false") {
    import TypeIdAndAttribute._
    val resource = Input(json"""{"type":"MyClass","id":"123","attributes":{"a":42}}""", focus ~> narrow[JObject])
    resource.succeedsWith(MyClass("MyClass", "123", 42))
  }

  describe("@Attribute only with allowUnknownFields = false (type field is unexpected)") {
    import AttributeOnlyStrict._
    val resource = Input(json"""{"type":"MyClass","attributes":{"a":42}}""", resourceType)
    resource.failsWith[MyClass](UnexpectedValue(_))
  }

  describe("@Id only with allowUnknownFields = false (type field is unexpected)") {
    import IdOnlyStrict._
    val resource = Input(json"""{"type":"MyClass","id":"123"}""", resourceType)
    resource.failsWith[MyClass](UnexpectedValue(_))
  }
}
