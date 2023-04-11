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

import cats.syntax.either._
import org.scalawag.bateman.json._
import org.scalawag.bateman.json.lens.{focus, _}
import org.scalawag.bateman.json.literal._
import org.scalawag.bateman.jsonapi.generic.Annotations.Type
import HListTypeDecoderTest._
import org.scalawag.bateman.jsonapi.generic.auto._
import org.scalawag.bateman.jsonapi.lens._

object HListTypeDecoderTest {
  object MyStringType {
    case class MyClass(@Type b: String)
  }
  object MyIntType {
    case class MyClass(@Type a: Int)
  }
  object MyOptionStringType {
    case class MyClass(@Type a: Option[String])
  }
  object MyOptionIntType {
    case class MyClass(@Type a: Option[Int])
  }
}

class HListTypeDecoderTest extends HListDecoderTestBase {
  private val nonObjectResource = Input(jsona"[]", focus)
  private val emptyResource = Input(json"{}", focus ~> narrow[JObject])
  private val nonStringType = Input(json"""{"type":{}}""", focus ~> "type" ~> narrow[JObject])
  private val nullTypeValue = Input(json"""{"type": null}""", focus ~> "type" ~> narrow[JNull])
  private val stringTypeValue = Input(json"""{"type":"MyClass"}""", resourceType)

  describe("MyStringType") {
    import MyStringType._

    nonObjectResource.failsWith[MyClass](JsonTypeMismatch(_, JObject))
    emptyResource.failsWith[MyClass](MissingField(_, "type"))
    nonStringType.failsWith[MyClass](JsonTypeMismatch(_, JString))
    nullTypeValue.failsWith[MyClass](JsonTypeMismatch(_, JString))
    stringTypeValue.succeedsWith(MyClass("MyClass"))
  }

  describe("MyIntType") {
    import MyIntType._

    // This is needed to turn a String type into an Int value
    implicit val stringToIntDecoder: JStringDecoder[Int] = _.value.value.length.rightNec

    nonObjectResource.failsWith[MyClass](JsonTypeMismatch(_, JObject))
    emptyResource.failsWith[MyClass](MissingField(_, "type"))
    nonStringType.failsWith[MyClass](JsonTypeMismatch(_, JString))
    nullTypeValue.failsWith[MyClass](JsonTypeMismatch(_, JString))
    stringTypeValue.succeedsWith(MyClass(7))
  }

  describe("MyOptionStringType") {
    import MyOptionStringType._

    nonObjectResource.failsWith[MyClass](JsonTypeMismatch(_, JObject))
    emptyResource.failsWith[MyClass](MissingField(_, "type"))
    nonStringType.failsWith[MyClass](JsonTypeMismatch(_, JString))
    nullTypeValue.failsWith[MyClass](JsonTypeMismatch(_, JString))
    stringTypeValue.succeedsWith(MyClass(Some("MyClass")))
  }

  describe("MyOptionIntType") {
    import MyOptionIntType._

    // This is needed to turn a String type into an Int value
    implicit val stringToIntDecoder: JStringDecoder[Int] = _.value.value.length.rightNec

    nonObjectResource.failsWith[MyClass](JsonTypeMismatch(_, JObject))
    emptyResource.failsWith[MyClass](MissingField(_, "type"))
    nonStringType.failsWith[MyClass](JsonTypeMismatch(_, JString))
    nullTypeValue.failsWith[MyClass](JsonTypeMismatch(_, JString))
    stringTypeValue.succeedsWith(MyClass(Some(7)))
  }
}
