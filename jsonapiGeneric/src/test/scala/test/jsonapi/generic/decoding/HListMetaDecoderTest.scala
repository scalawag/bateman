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
import org.scalawag.bateman.json.lens.{focus, _}
import org.scalawag.bateman.json.literal._
import org.scalawag.bateman.jsonapi.generic.Annotations.Meta
import HListMetaDecoderTest._
import org.scalawag.bateman.jsonapi.generic.auto._
import org.scalawag.bateman.jsonapi.lens._
import org.scalawag.bateman.json.focus.weak._

object HListMetaDecoderTest {
  object MyIdMeta {
    case class MyClass(@Meta a: Int)
  }
  object MyNullableMeta {
    case class MyClass(@Meta a: Nullable[Int])
  }
  object MyListMeta {
    case class MyClass(@Meta a: List[Int])
  }
  object MyOptionIdMeta {
    case class MyClass(@Meta a: Option[Int])
  }
  object MyOptionNullableMeta {
    case class MyClass(@Meta a: Option[Nullable[Int]])
  }
  object MyOptionListMeta {
    case class MyClass(@Meta a: Option[List[Int]])
  }
  object MyIdMetaWithDefault {
    case class MyClass(@Meta a: Int = 417)
  }
  object MyNullableMetaWithDefault {
    case class MyClass(@Meta a: Nullable[Int] = NotNull(5))
  }
  object MyListMetaWithDefault {
    case class MyClass(@Meta a: List[Int] = List(-1, 0, 1, 2))
  }
  object MyOptionIdMetaWithDefault {
    case class MyClass(@Meta a: Option[Int] = Some(7))
  }
  object MyOptionNullableMetaWithDefault {
    case class MyClass(@Meta a: Option[Nullable[Int]] = Some(NotNull(23)))
  }
  object MyOptionListMetaWithDefault {
    case class MyClass(@Meta a: Option[List[Int]] = Some(List(23, 29)))
  }
}

class HListMetaDecoderTest extends HListDecoderTestBase {
  private val nonObjectResource = Input(jsona"[]", focus)

  private val emptyResource = Input(json"""
    {
      "type": "MyClass"
    }
  """, focus ~> narrow[JObject])

  private val nonObjectMeta = Input(json"""
    {
      "type": "MyClass",
      "meta":"foo"
    }
  """, focus ~> "meta")

  private val emptyMeta = Input(json"""
    {
      "type": "MyClass",
      "meta": {}
    }
  """, meta)

  private val booleanMetaValue = Input(json"""
    {
      "type": "MyClass",
      "meta": {
        "a": true
      }
    }
  """, meta("a"))

  private val nullMetaValue = Input(json"""
    {
      "type": "MyClass",
      "meta": {
        "a": null
      }
    }
  """, meta("a") ~> narrow[JNull])

  private val intMetaValue = Input(json"""
    {
      "type": "MyClass",
      "meta": {
        "a": -17
      }
    }
  """, meta("a"))

  private val intArrayMetaValue = Input(json"""
    {
      "type": "MyClass",
      "meta": {
        "a": [2,3,5,7,11]
      }
    }
  """, meta("a"))

  private val mixedArrayMetaValue = Input(json"""
    {
      "type": "MyClass",
      "meta": {
        "a": [2,"3",5,null]
      }
    }
  """, meta("a") ~> narrow[JArray])

  describe("MyIdMeta") {
    import MyIdMeta._

    nonObjectResource.failsWith[MyClass](JsonTypeMismatch(_, JObject))
    emptyResource.failsWith[MyClass](MissingField(_, "meta"))
    nonObjectMeta.failsWith[MyClass](JsonTypeMismatch(_, JObject))
    emptyMeta.failsWith[MyClass](MissingField(_, "a"))
    booleanMetaValue.failsWith[MyClass](JsonTypeMismatch(_, JNumber))
    nullMetaValue.failsWith[MyClass](JsonTypeMismatch(_, JNumber))
    intMetaValue.succeedsWith(MyClass(-17))
    intArrayMetaValue.failsWith[MyClass](JsonTypeMismatch(_, JNumber))
    mixedArrayMetaValue.failsWith[MyClass](JsonTypeMismatch(_, JNumber))
  }

  describe("MyNullableMeta") {
    import MyNullableMeta._

    nonObjectResource.failsWith[MyClass](JsonTypeMismatch(_, JObject))
    emptyResource.failsWith[MyClass](MissingField(_, "meta"))
    nonObjectMeta.failsWith[MyClass](JsonTypeMismatch(_, JObject))
    emptyMeta.failsWith[MyClass](MissingField(_, "a"))
    booleanMetaValue.failsWith[MyClass](JsonTypeMismatch(_, JNull, JNumber))
    nullMetaValue.succeedsWith(MyClass(Null))
    intMetaValue.succeedsWith(MyClass(NotNull(-17)))
    intArrayMetaValue.failsWith[MyClass](JsonTypeMismatch(_, JNull, JNumber))
    mixedArrayMetaValue.failsWith[MyClass](JsonTypeMismatch(_, JNull, JNumber))
  }

  describe("MyListMeta") {
    import MyListMeta._

    nonObjectResource.failsWith[MyClass](JsonTypeMismatch(_, JObject))
    emptyResource.failsWith[MyClass](MissingField(_, "meta"))
    nonObjectMeta.failsWith[MyClass](JsonTypeMismatch(_, JObject))
    emptyMeta.failsWith[MyClass](MissingField(_, "a"))
    booleanMetaValue.failsWith[MyClass](JsonTypeMismatch(_, JArray))
    nullMetaValue.failsWith[MyClass](JsonTypeMismatch(_, JArray))
    intMetaValue.failsWith[MyClass](JsonTypeMismatch(_, JArray))
    intArrayMetaValue.succeedsWith(MyClass(List(2, 3, 5, 7, 11)))
    mixedArrayMetaValue.failsWithMultiple[MyClass] { attr =>
      NonEmptyChain(
        JsonTypeMismatch(attr.item(1).shouldSucceed, JNumber),
        JsonTypeMismatch(attr.item(3).shouldSucceed, JNumber)
      )
    }
  }

  describe("MyIdMetaWithDefault") {
    import MyIdMetaWithDefault._

    nonObjectResource.failsWith[MyClass](JsonTypeMismatch(_, JObject))
    emptyResource.succeedsWith(MyClass(417))
    nonObjectMeta.failsWith[MyClass](JsonTypeMismatch(_, JObject))
    emptyMeta.succeedsWith(MyClass(417))
    booleanMetaValue.failsWith[MyClass](JsonTypeMismatch(_, JNumber))
    nullMetaValue.failsWith[MyClass](JsonTypeMismatch(_, JNumber))
    intMetaValue.succeedsWith(MyClass(-17))
    intArrayMetaValue.failsWith[MyClass](JsonTypeMismatch(_, JNumber))
    mixedArrayMetaValue.failsWith[MyClass](JsonTypeMismatch(_, JNumber))
  }

  describe("MyNullableMetaWithDefault") {
    import MyNullableMetaWithDefault._

    nonObjectResource.failsWith[MyClass](JsonTypeMismatch(_, JObject))
    emptyResource.succeedsWith(MyClass(NotNull(5)))
    nonObjectMeta.failsWith[MyClass](JsonTypeMismatch(_, JObject))
    emptyMeta.succeedsWith(MyClass(NotNull(5)))
    booleanMetaValue.failsWith[MyClass](JsonTypeMismatch(_, JNull, JNumber))
    nullMetaValue.succeedsWith(MyClass(Null))
    intMetaValue.succeedsWith(MyClass(NotNull(-17)))
    intArrayMetaValue.failsWith[MyClass](JsonTypeMismatch(_, JNull, JNumber))
    mixedArrayMetaValue.failsWith[MyClass](JsonTypeMismatch(_, JNull, JNumber))
  }

  describe("MyListMetaWithDefault") {
    import MyListMetaWithDefault._

    nonObjectResource.failsWith[MyClass](JsonTypeMismatch(_, JObject))
    emptyResource.succeedsWith(MyClass(List(-1, 0, 1, 2)))
    nonObjectMeta.failsWith[MyClass](JsonTypeMismatch(_, JObject))
    emptyMeta.succeedsWith(MyClass(List(-1, 0, 1, 2)))
    booleanMetaValue.failsWith[MyClass](JsonTypeMismatch(_, JArray))
    nullMetaValue.failsWith[MyClass](JsonTypeMismatch(_, JArray))
    intMetaValue.failsWith[MyClass](JsonTypeMismatch(_, JArray))
    intArrayMetaValue.succeedsWith(MyClass(List(2, 3, 5, 7, 11)))
    mixedArrayMetaValue.failsWithMultiple[MyClass] { attr =>
      NonEmptyChain(
        JsonTypeMismatch(attr.item(1).shouldSucceed, JNumber),
        JsonTypeMismatch(attr.item(3).shouldSucceed, JNumber)
      )
    }
  }

  describe("MyOptionIdMeta") {
    import MyOptionIdMeta._

    nonObjectResource.failsWith[MyClass](JsonTypeMismatch(_, JObject))
    emptyResource.succeedsWith(MyClass(None))
    nonObjectMeta.failsWith[MyClass](JsonTypeMismatch(_, JObject))
    emptyMeta.succeedsWith(MyClass(None))
    booleanMetaValue.failsWith[MyClass](JsonTypeMismatch(_, JNumber))
    nullMetaValue.failsWith[MyClass](JsonTypeMismatch(_, JNumber))
    intMetaValue.succeedsWith(MyClass(Some(-17)))
    intArrayMetaValue.failsWith[MyClass](JsonTypeMismatch(_, JNumber))
    mixedArrayMetaValue.failsWith[MyClass](JsonTypeMismatch(_, JNumber))
  }

  describe("MyOptionNullableMeta") {
    import MyOptionNullableMeta._

    nonObjectResource.failsWith[MyClass](JsonTypeMismatch(_, JObject))
    emptyResource.succeedsWith(MyClass(None))
    nonObjectMeta.failsWith[MyClass](JsonTypeMismatch(_, JObject))
    emptyMeta.succeedsWith(MyClass(None))
    booleanMetaValue.failsWith[MyClass](JsonTypeMismatch(_, JNull, JNumber))
    nullMetaValue.succeedsWith(MyClass(Some(Null)))
    intMetaValue.succeedsWith(MyClass(Some(NotNull(-17))))
    intArrayMetaValue.failsWith[MyClass](JsonTypeMismatch(_, JNull, JNumber))
    mixedArrayMetaValue.failsWith[MyClass](JsonTypeMismatch(_, JNull, JNumber))
  }

  describe("MyOptionListMeta") {
    import MyOptionListMeta._

    nonObjectResource.failsWith[MyClass](JsonTypeMismatch(_, JObject))
    emptyResource.succeedsWith(MyClass(None))
    nonObjectMeta.failsWith[MyClass](JsonTypeMismatch(_, JObject))
    emptyMeta.succeedsWith(MyClass(None))
    booleanMetaValue.failsWith[MyClass](JsonTypeMismatch(_, JArray))
    nullMetaValue.failsWith[MyClass](JsonTypeMismatch(_, JArray))
    intMetaValue.failsWith[MyClass](JsonTypeMismatch(_, JArray))
    intArrayMetaValue.succeedsWith(MyClass(Some(List(2, 3, 5, 7, 11))))
    mixedArrayMetaValue.failsWithMultiple[MyClass] { attr =>
      NonEmptyChain(
        JsonTypeMismatch(attr.item(1).shouldSucceed, JNumber),
        JsonTypeMismatch(attr.item(3).shouldSucceed, JNumber)
      )
    }
  }

  describe("MyOptionIdMetaWithDefault") {
    import MyOptionIdMetaWithDefault._

    nonObjectResource.failsWith[MyClass](JsonTypeMismatch(_, JObject))
    emptyResource.succeedsWith(MyClass(None))
    nonObjectMeta.failsWith[MyClass](JsonTypeMismatch(_, JObject))
    emptyMeta.succeedsWith(MyClass(None))
    booleanMetaValue.failsWith[MyClass](JsonTypeMismatch(_, JNumber))
    nullMetaValue.failsWith[MyClass](JsonTypeMismatch(_, JNumber))
    intMetaValue.succeedsWith(MyClass(Some(-17)))
    intArrayMetaValue.failsWith[MyClass](JsonTypeMismatch(_, JNumber))
    mixedArrayMetaValue.failsWith[MyClass](JsonTypeMismatch(_, JNumber))
  }

  describe("MyOptionNullableMetaWithDefault") {
    import MyOptionNullableMetaWithDefault._

    nonObjectResource.failsWith[MyClass](JsonTypeMismatch(_, JObject))
    emptyResource.succeedsWith(MyClass(None))
    nonObjectMeta.failsWith[MyClass](JsonTypeMismatch(_, JObject))
    emptyMeta.succeedsWith(MyClass(None))
    booleanMetaValue.failsWith[MyClass](JsonTypeMismatch(_, JNull, JNumber))
    nullMetaValue.succeedsWith(MyClass(Some(Null)))
    intMetaValue.succeedsWith(MyClass(Some(NotNull(-17))))
    intArrayMetaValue.failsWith[MyClass](JsonTypeMismatch(_, JNull, JNumber))
    mixedArrayMetaValue.failsWith[MyClass](JsonTypeMismatch(_, JNull, JNumber))
  }

  describe("MyOptionListMetaWithDefault") {
    import MyOptionListMetaWithDefault._

    nonObjectResource.failsWith[MyClass](JsonTypeMismatch(_, JObject))
    emptyResource.succeedsWith(MyClass(None))
    nonObjectMeta.failsWith[MyClass](JsonTypeMismatch(_, JObject))
    emptyMeta.succeedsWith(MyClass(None))
    booleanMetaValue.failsWith[MyClass](JsonTypeMismatch(_, JArray))
    nullMetaValue.failsWith[MyClass](JsonTypeMismatch(_, JArray))
    intMetaValue.failsWith[MyClass](JsonTypeMismatch(_, JArray))
    intArrayMetaValue.succeedsWith(MyClass(Some(List(2, 3, 5, 7, 11))))
    mixedArrayMetaValue.failsWithMultiple[MyClass] { attr =>
      NonEmptyChain(
        JsonTypeMismatch(attr.item(1).shouldSucceed, JNumber),
        JsonTypeMismatch(attr.item(3).shouldSucceed, JNumber)
      )
    }
  }
}
