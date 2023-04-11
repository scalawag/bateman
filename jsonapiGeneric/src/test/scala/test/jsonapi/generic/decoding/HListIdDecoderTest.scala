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

import org.scalawag.bateman.json._
import org.scalawag.bateman.json.lens.{focus, _}
import org.scalawag.bateman.json.literal._
import org.scalawag.bateman.jsonapi.generic.Annotations.Id
import HListIdDecoderTest._
import org.scalawag.bateman.jsonapi.generic.auto._
import org.scalawag.bateman.jsonapi.lens._

object HListIdDecoderTest {
  object MyStringId {
    case class MyClass(@Id b: String)
  }
  object MyIntId {
    case class MyClass(@Id a: Int)
  }
  object MyOptionStringId {
    case class MyClass(@Id a: Option[String])
  }
  object MyOptionIntId {
    case class MyClass(@Id a: Option[Int])
  }
}

class HListIdDecoderTest extends HListDecoderTestBase {
  private val nonObjectResource = Input(jsona"[]", focus)

  private val emptyResource = Input(json"""
    {
      "type": "MyClass"
    }
  """, focus ~> narrow[JObject])

  private val nonStringId = Input(json"""
    {
      "type": "MyClass",
      "id": {}
    }
  """, focus ~> "id" ~> narrow[JObject])

  private val nullIdValue = Input(json"""
    {
      "type": "MyClass",
      "id": null
    }
  """, focus ~> "id" ~> narrow[JNull])

  private val fooIdValue = Input(json"""
    {
      "type": "MyClass",
      "id": "foo"
    }
  """, id)

  private val intIdValue = Input(json"""
    {
      "type": "MyClass",
      "id":"11"
    }
  """, id)

  // This is needed to turn a String id into an Int
  implicit val stringToIntDecoder: JStringDecoder[Int] =
    Decoder.jstringToJNumber.andThen(Decoder.jnumberToIntDecoder)

  describe("MyStringId") {
    import MyStringId._

    nonObjectResource.failsWith[MyClass](JsonTypeMismatch(_, JObject))
    emptyResource.failsWith[MyClass](MissingField(_, "id"))
    nonStringId.failsWith[MyClass](JsonTypeMismatch(_, JString))
    nullIdValue.failsWith[MyClass](JsonTypeMismatch(_, JString))
    fooIdValue.succeedsWith(MyClass("foo"))
    intIdValue.succeedsWith(MyClass("11"))
  }

  describe("MyIntId") {
    import MyIntId._

    nonObjectResource.failsWith[MyClass](JsonTypeMismatch(_, JObject))
    emptyResource.failsWith[MyClass](MissingField(_, "id"))
    nonStringId.failsWith[MyClass](JsonTypeMismatch(_, JString))
    nullIdValue.failsWith[MyClass](JsonTypeMismatch(_, JString))
    fooIdValue.failsWith[MyClass](InvalidValue(_, "'foo' is not a valid JSON number"))
    intIdValue.succeedsWith(MyClass(11))
  }

  describe("MyOptionStringId") {
    import MyOptionStringId._

    nonObjectResource.failsWith[MyClass](JsonTypeMismatch(_, JObject))
    emptyResource.succeedsWith(MyClass(None))
    nonStringId.failsWith[MyClass](JsonTypeMismatch(_, JString))
    nullIdValue.failsWith[MyClass](JsonTypeMismatch(_, JString))
    fooIdValue.succeedsWith(MyClass(Some("foo")))
    intIdValue.succeedsWith(MyClass(Some("11")))
  }

  describe("MyOptionIntId") {
    import MyOptionIntId._

    nonObjectResource.failsWith[MyClass](JsonTypeMismatch(_, JObject))
    emptyResource.succeedsWith(MyClass(None))
    nonStringId.failsWith[MyClass](JsonTypeMismatch(_, JString))
    nullIdValue.failsWith[MyClass](JsonTypeMismatch(_, JString))
    fooIdValue.failsWith[MyClass](InvalidValue(_, "'foo' is not a valid JSON number"))
    intIdValue.succeedsWith(MyClass(Some(11)))
  }
}
