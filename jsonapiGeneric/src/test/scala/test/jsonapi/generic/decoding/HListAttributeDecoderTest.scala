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

import cats.data.NonEmptyChain
import org.scalawag.bateman.json._
import org.scalawag.bateman.json.focus.weak._
import org.scalawag.bateman.json.lens.{focus, _}
import org.scalawag.bateman.json.literal._
import org.scalawag.bateman.jsonapi.lens._
import org.scalawag.bateman.jsonapi.generic.auto._
import HListAttributeDecoderTest._
import org.scalawag.bateman.jsonapi.generic.Annotations._

object HListAttributeDecoderTest {
  object MyIdAttribute {
    case class MyClass(@Attribute a: Int)
  }
  object MyNullableAttribute {
    case class MyClass(@Attribute a: Nullable[Int])
  }
  object MyListAttribute {
    case class MyClass(@Attribute a: List[Int])
  }
  object MyOptionIdAttribute {
    case class MyClass(@Attribute a: Option[Int])
  }
  object MyOptionNullableAttribute {
    case class MyClass(@Attribute a: Option[Nullable[Int]])
  }
  object MyOptionListAttribute {
    case class MyClass(@Attribute a: Option[List[Int]])
  }
  object MyIdAttributeWithDefault {
    case class MyClass(@Attribute a: Int = 417)
  }
  object MyNullableAttributeWithDefault {
    case class MyClass(@Attribute a: Nullable[Int] = NotNull(5))
  }
  object MyListAttributeWithDefault {
    case class MyClass(@Attribute a: List[Int] = List(-1, 0, 1, 2))
  }
  object MyOptionIdAttributeWithDefault {
    case class MyClass(@Attribute a: Option[Int] = Some(7))
  }
  object MyOptionNullableAttributeWithDefault {
    case class MyClass(@Attribute a: Option[Nullable[Int]] = Some(NotNull(23)))
  }
  object MyOptionListAttributeWithDefault {
    case class MyClass(@Attribute a: Option[List[Int]] = Some(List(23, 29)))
  }
}

class HListAttributeDecoderTest extends HListDecoderTestBase {
  private val nonObjectResource = Input(jsona"[]", focus)

  private val emptyResource = Input(json"""
    {
      "type": "MyClass"
    }
  """, focus ~> narrow[JObject])

  private val nonObjectAttributes = Input(json"""
    {
      "type": "MyClass",
      "attributes":"foo"
    }
  """, focus ~> "attributes")

  private val emptyAttributes = Input(json"""
    {
      "type": "MyClass",
      "attributes": {}
    }
  """, attributes)

  private val booleanAttributeValue = Input(json"""
    {
      "type": "MyClass",
      "attributes": {
        "a": true
      }
    }
  """, attribute("a"))

  private val nullAttributeValue = Input(json"""
    {
      "type": "MyClass",
      "attributes": {
        "a": null
      }
    }
  """, attribute("a") ~> narrow[JNull])

  private val intAttributeValue = Input(json"""
    {
      "type": "MyClass",
      "attributes": {
        "a": -17
      }
    }
  """, attribute("a"))

  private val intArrayAttributeValue = Input(json"""
    {
      "type": "MyClass",
      "attributes": {
        "a": [2,3,5,7,11]
      }
    }
  """, attribute("a"))

  private val mixedArrayAttributeValue = Input(json"""
    {
      "type": "MyClass",
      "attributes": {
        "a": [2,"3",5,null]
      }
    }
  """, attribute("a") ~> narrow[JArray])

  describe("MyIdAttribute") {
    import MyIdAttribute._

    nonObjectResource.failsWith[MyClass](JsonTypeMismatch(_, JObject))
    emptyResource.failsWith[MyClass](MissingField(_, "attributes"))
    nonObjectAttributes.failsWith[MyClass](JsonTypeMismatch(_, JObject))
    emptyAttributes.failsWith[MyClass](MissingField(_, "a"))
    booleanAttributeValue.failsWith[MyClass](JsonTypeMismatch(_, JNumber))
    nullAttributeValue.failsWith[MyClass](JsonTypeMismatch(_, JNumber))
    intAttributeValue.succeedsWith(MyClass(-17))
    intArrayAttributeValue.failsWith[MyClass](JsonTypeMismatch(_, JNumber))
    mixedArrayAttributeValue.failsWith[MyClass](JsonTypeMismatch(_, JNumber))
  }

  describe("MyNullableAttribute") {
    import MyNullableAttribute._

    nonObjectResource.failsWith[MyClass](JsonTypeMismatch(_, JObject))
    emptyResource.failsWith[MyClass](MissingField(_, "attributes"))
    nonObjectAttributes.failsWith[MyClass](JsonTypeMismatch(_, JObject))
    emptyAttributes.failsWith[MyClass](MissingField(_, "a"))
    booleanAttributeValue.failsWith[MyClass](JsonTypeMismatch(_, JNull, JNumber))
    nullAttributeValue.succeedsWith(MyClass(Null))
    intAttributeValue.succeedsWith(MyClass(NotNull(-17)))
    intArrayAttributeValue.failsWith[MyClass](JsonTypeMismatch(_, JNull, JNumber))
    mixedArrayAttributeValue.failsWith[MyClass](JsonTypeMismatch(_, JNull, JNumber))
  }

  describe("MyListAttribute") {
    import MyListAttribute._

    nonObjectResource.failsWith[MyClass](JsonTypeMismatch(_, JObject))
    emptyResource.failsWith[MyClass](MissingField(_, "attributes"))
    nonObjectAttributes.failsWith[MyClass](JsonTypeMismatch(_, JObject))
    emptyAttributes.failsWith[MyClass](MissingField(_, "a"))
    booleanAttributeValue.failsWith[MyClass](JsonTypeMismatch(_, JArray))
    nullAttributeValue.failsWith[MyClass](JsonTypeMismatch(_, JArray))
    intAttributeValue.failsWith[MyClass](JsonTypeMismatch(_, JArray))
    intArrayAttributeValue.succeedsWith(MyClass(List(2, 3, 5, 7, 11)))
    mixedArrayAttributeValue.failsWithMultiple[MyClass] { attr =>
      NonEmptyChain(
        JsonTypeMismatch(attr.item(1).shouldSucceed, JNumber),
        JsonTypeMismatch(attr.item(3).shouldSucceed, JNumber)
      )
    }
  }

  describe("MyIdAttributeWithDefault") {
    import MyIdAttributeWithDefault._

    nonObjectResource.failsWith[MyClass](JsonTypeMismatch(_, JObject))
    emptyResource.succeedsWith(MyClass(417))
    nonObjectAttributes.failsWith[MyClass](JsonTypeMismatch(_, JObject))
    emptyAttributes.succeedsWith(MyClass(417))
    booleanAttributeValue.failsWith[MyClass](JsonTypeMismatch(_, JNumber))
    nullAttributeValue.failsWith[MyClass](JsonTypeMismatch(_, JNumber))
    intAttributeValue.succeedsWith(MyClass(-17))
    intArrayAttributeValue.failsWith[MyClass](JsonTypeMismatch(_, JNumber))
    mixedArrayAttributeValue.failsWith[MyClass](JsonTypeMismatch(_, JNumber))
  }

  describe("MyNullableAttributeWithDefault") {
    import MyNullableAttributeWithDefault._

    nonObjectResource.failsWith[MyClass](JsonTypeMismatch(_, JObject))
    emptyResource.succeedsWith(MyClass(NotNull(5)))
    nonObjectAttributes.failsWith[MyClass](JsonTypeMismatch(_, JObject))
    emptyAttributes.succeedsWith(MyClass(NotNull(5)))
    booleanAttributeValue.failsWith[MyClass](JsonTypeMismatch(_, JNull, JNumber))
    nullAttributeValue.succeedsWith(MyClass(Null))
    intAttributeValue.succeedsWith(MyClass(NotNull(-17)))
    intArrayAttributeValue.failsWith[MyClass](JsonTypeMismatch(_, JNull, JNumber))
    mixedArrayAttributeValue.failsWith[MyClass](JsonTypeMismatch(_, JNull, JNumber))
  }

  describe("MyListAttributeWithDefault") {
    import MyListAttributeWithDefault._

    nonObjectResource.failsWith[MyClass](JsonTypeMismatch(_, JObject))
    emptyResource.succeedsWith(MyClass(List(-1, 0, 1, 2)))
    nonObjectAttributes.failsWith[MyClass](JsonTypeMismatch(_, JObject))
    emptyAttributes.succeedsWith(MyClass(List(-1, 0, 1, 2)))
    booleanAttributeValue.failsWith[MyClass](JsonTypeMismatch(_, JArray))
    nullAttributeValue.failsWith[MyClass](JsonTypeMismatch(_, JArray))
    intAttributeValue.failsWith[MyClass](JsonTypeMismatch(_, JArray))
    intArrayAttributeValue.succeedsWith(MyClass(List(2, 3, 5, 7, 11)))
    mixedArrayAttributeValue.failsWithMultiple[MyClass] { attr =>
      NonEmptyChain(
        JsonTypeMismatch(attr.item(1).shouldSucceed, JNumber),
        JsonTypeMismatch(attr.item(3).shouldSucceed, JNumber)
      )
    }
  }

  describe("MyOptionIdAttribute") {
    import MyOptionIdAttribute._

    nonObjectResource.failsWith[MyClass](JsonTypeMismatch(_, JObject))
    emptyResource.succeedsWith(MyClass(None))
    nonObjectAttributes.failsWith[MyClass](JsonTypeMismatch(_, JObject))
    emptyAttributes.succeedsWith(MyClass(None))
    booleanAttributeValue.failsWith[MyClass](JsonTypeMismatch(_, JNumber))
    nullAttributeValue.failsWith[MyClass](JsonTypeMismatch(_, JNumber))
    intAttributeValue.succeedsWith(MyClass(Some(-17)))
    intArrayAttributeValue.failsWith[MyClass](JsonTypeMismatch(_, JNumber))
    mixedArrayAttributeValue.failsWith[MyClass](JsonTypeMismatch(_, JNumber))
  }

  describe("MyOptionNullableAttribute") {
    import MyOptionNullableAttribute._

    nonObjectResource.failsWith[MyClass](JsonTypeMismatch(_, JObject))
    emptyResource.succeedsWith(MyClass(None))
    nonObjectAttributes.failsWith[MyClass](JsonTypeMismatch(_, JObject))
    emptyAttributes.succeedsWith(MyClass(None))
    booleanAttributeValue.failsWith[MyClass](JsonTypeMismatch(_, JNull, JNumber))
    nullAttributeValue.succeedsWith(MyClass(Some(Null)))
    intAttributeValue.succeedsWith(MyClass(Some(NotNull(-17))))
    intArrayAttributeValue.failsWith[MyClass](JsonTypeMismatch(_, JNull, JNumber))
    mixedArrayAttributeValue.failsWith[MyClass](JsonTypeMismatch(_, JNull, JNumber))
  }

  describe("MyOptionListAttribute") {
    import MyOptionListAttribute._

    nonObjectResource.failsWith[MyClass](JsonTypeMismatch(_, JObject))
    emptyResource.succeedsWith(MyClass(None))
    nonObjectAttributes.failsWith[MyClass](JsonTypeMismatch(_, JObject))
    emptyAttributes.succeedsWith(MyClass(None))
    booleanAttributeValue.failsWith[MyClass](JsonTypeMismatch(_, JArray))
    nullAttributeValue.failsWith[MyClass](JsonTypeMismatch(_, JArray))
    intAttributeValue.failsWith[MyClass](JsonTypeMismatch(_, JArray))
    intArrayAttributeValue.succeedsWith(MyClass(Some(List(2, 3, 5, 7, 11))))
    mixedArrayAttributeValue.failsWithMultiple[MyClass] { attr =>
      NonEmptyChain(
        JsonTypeMismatch(attr.item(1).shouldSucceed, JNumber),
        JsonTypeMismatch(attr.item(3).shouldSucceed, JNumber)
      )
    }
  }

  describe("MyOptionIdAttributeWithDefault") {
    import MyOptionIdAttributeWithDefault._

    nonObjectResource.failsWith[MyClass](JsonTypeMismatch(_, JObject))
    emptyResource.succeedsWith(MyClass(None))
    nonObjectAttributes.failsWith[MyClass](JsonTypeMismatch(_, JObject))
    emptyAttributes.succeedsWith(MyClass(None))
    booleanAttributeValue.failsWith[MyClass](JsonTypeMismatch(_, JNumber))
    nullAttributeValue.failsWith[MyClass](JsonTypeMismatch(_, JNumber))
    intAttributeValue.succeedsWith(MyClass(Some(-17)))
    intArrayAttributeValue.failsWith[MyClass](JsonTypeMismatch(_, JNumber))
    mixedArrayAttributeValue.failsWith[MyClass](JsonTypeMismatch(_, JNumber))
  }

  describe("MyOptionNullableAttributeWithDefault") {
    import MyOptionNullableAttributeWithDefault._

    nonObjectResource.failsWith[MyClass](JsonTypeMismatch(_, JObject))
    emptyResource.succeedsWith(MyClass(None))
    nonObjectAttributes.failsWith[MyClass](JsonTypeMismatch(_, JObject))
    emptyAttributes.succeedsWith(MyClass(None))
    booleanAttributeValue.failsWith[MyClass](JsonTypeMismatch(_, JNull, JNumber))
    nullAttributeValue.succeedsWith(MyClass(Some(Null)))
    intAttributeValue.succeedsWith(MyClass(Some(NotNull(-17))))
    intArrayAttributeValue.failsWith[MyClass](JsonTypeMismatch(_, JNull, JNumber))
    mixedArrayAttributeValue.failsWith[MyClass](JsonTypeMismatch(_, JNull, JNumber))
  }

  describe("MyOptionListAttributeWithDefault") {
    import MyOptionListAttributeWithDefault._

    nonObjectResource.failsWith[MyClass](JsonTypeMismatch(_, JObject))
    emptyResource.succeedsWith(MyClass(None))
    nonObjectAttributes.failsWith[MyClass](JsonTypeMismatch(_, JObject))
    emptyAttributes.succeedsWith(MyClass(None))
    booleanAttributeValue.failsWith[MyClass](JsonTypeMismatch(_, JArray))
    nullAttributeValue.failsWith[MyClass](JsonTypeMismatch(_, JArray))
    intAttributeValue.failsWith[MyClass](JsonTypeMismatch(_, JArray))
    intArrayAttributeValue.succeedsWith(MyClass(Some(List(2, 3, 5, 7, 11))))
    mixedArrayAttributeValue.failsWithMultiple[MyClass] { attr =>
      NonEmptyChain(
        JsonTypeMismatch(attr.item(1).shouldSucceed, JNumber),
        JsonTypeMismatch(attr.item(3).shouldSucceed, JNumber)
      )
    }
  }
}
