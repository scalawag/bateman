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

package test.json.generic.decoding

import org.scalawag.bateman.json.generic.Config
import org.scalawag.bateman.json.generic.naming.{CamelCase, CaseTransformation, PascalCase}
import org.scalawag.bateman.json.generic.semiauto.unchecked._
import org.scalawag.bateman.json.lens.{focus, _}
import org.scalawag.bateman.json.literal._
import org.scalawag.bateman.json.{JNumber, _}

object FieldDecoderTest {
  object MyBareField {
    case class MyClass(a: Int)
    implicit val decoderForMyClass: JObjectDecoder[MyClass] = deriveDecoderForCaseClass[MyClass]()
  }
  object MyOptionField {
    case class MyClass(a: Option[Int])
    implicit val decoderForMyClass: JObjectDecoder[MyClass] = deriveDecoderForCaseClass[MyClass]()
  }
  object MyNullableField {
    case class MyClass(a: Nullable[Int])
    implicit val decoderForMyClass: JObjectDecoder[MyClass] = deriveDecoderForCaseClass[MyClass]()
  }
  object MyOptionNullableField {
    case class MyClass(a: Option[Nullable[Int]])
    implicit val decoderForMyClass: JObjectDecoder[MyClass] = deriveDecoderForCaseClass[MyClass]()
  }
  object MyBareFieldWithDefault {
    case class MyClass(a: Int = 417)
    implicit val decoderForMyClass: JObjectDecoder[MyClass] = deriveDecoderForCaseClass[MyClass]()
  }
  object MyOptionFieldWithDefault {
    case class MyClass(a: Option[Int] = Some(7))
    implicit val decoderForMyClass: JObjectDecoder[MyClass] = deriveDecoderForCaseClass[MyClass]()
  }
  object MyNullableFieldWithDefault {
    case class MyClass(a: Nullable[Int] = NotNull(7))
    implicit val decoderForMyClass: JObjectDecoder[MyClass] = deriveDecoderForCaseClass[MyClass]()
  }
  object MyOptionNullableFieldWithDefault {
    case class MyClass(a: Option[Nullable[Int]] = Some(NotNull(7)))
    implicit val decoderForMyClass: JObjectDecoder[MyClass] = deriveDecoderForCaseClass[MyClass]()
  }
}

class FieldDecoderTest extends DecoderTestBase {
  private val emptyArray = Input(jsona"[]", focus)
  private val emptyObject = Input(json"{}", focus ~> narrow[JObject])
  private val stringType = Input(json"""{"a": "oops"}""", "a")
  private val nullField = Input(json"""{"a": null}""", "a")
  private val intField = Input(json"""{"a": -17}""", "a")

  describe("MyBareField") {
    import FieldDecoderTest.MyBareField._

    emptyArray.failsWith[MyClass](JsonTypeMismatch(_, JObject))
    emptyObject.failsWith[MyClass](MissingField(_, "a"))
    stringType.failsWith[MyClass](JsonTypeMismatch(_, JNumber))
    nullField.failsWith[MyClass](JsonTypeMismatch(_, JNumber))
    intField.succeedsWith(MyClass(-17))
  }

  describe("MyOptionField") {
    import FieldDecoderTest.MyOptionField._

    emptyArray.failsWith[MyClass](JsonTypeMismatch(_, JObject))
    emptyObject.succeedsWith(MyClass(None))
    stringType.failsWith[MyClass](JsonTypeMismatch(_, JNumber))
    nullField.failsWith[MyClass](JsonTypeMismatch(_, JNumber))
    intField.succeedsWith(MyClass(Some(-17)))
  }

  describe("MyNullableField") {
    import FieldDecoderTest.MyNullableField._

    emptyArray.failsWith[MyClass](JsonTypeMismatch(_, JObject))
    emptyObject.failsWith[MyClass](MissingField(_, "a"))
    stringType.failsWith[MyClass](JsonTypeMismatch(_, JNumber, JNull))
    nullField.succeedsWith(MyClass(Null))
    intField.succeedsWith(MyClass(NotNull(-17)))
  }

  describe("MyOptionNullableField") {
    import FieldDecoderTest.MyOptionNullableField._

    emptyArray.failsWith[MyClass](JsonTypeMismatch(_, JObject))
    emptyObject.succeedsWith(MyClass(None))
    stringType.failsWith[MyClass](JsonTypeMismatch(_, JNumber, JNull))
    nullField.succeedsWith(MyClass(Some(Null)))
    intField.succeedsWith(MyClass(Some(NotNull(-17))))
  }

  describe("MyBareFieldWithDefault") {
    import FieldDecoderTest.MyBareFieldWithDefault._

    emptyArray.failsWith[MyClass](JsonTypeMismatch(_, JObject))
    emptyObject.succeedsWith(MyClass(417))
    stringType.failsWith[MyClass](JsonTypeMismatch(_, JNumber))
    nullField.failsWith[MyClass](JsonTypeMismatch(_, JNumber))
    intField.succeedsWith(MyClass(-17))
  }

  describe("MyOptionFieldWithDefault") {
    import FieldDecoderTest.MyOptionFieldWithDefault._

    emptyArray.failsWith[MyClass](JsonTypeMismatch(_, JObject))
    emptyObject.succeedsWith(MyClass(None))
    stringType.failsWith[MyClass](JsonTypeMismatch(_, JNumber))
    nullField.failsWith[MyClass](JsonTypeMismatch(_, JNumber))
    intField.succeedsWith(MyClass(Some(-17)))
  }

  describe("MyNullableFieldWithDefault") {
    import FieldDecoderTest.MyNullableFieldWithDefault._

    emptyArray.failsWith[MyClass](JsonTypeMismatch(_, JObject))
    emptyObject.succeedsWith(MyClass(NotNull(7)))
    stringType.failsWith[MyClass](JsonTypeMismatch(_, JNumber, JNull))
    nullField.succeedsWith(MyClass(Null))
    intField.succeedsWith(MyClass(NotNull(-17)))
  }

  describe("MyOptionNullableFieldWithDefault") {
    import FieldDecoderTest.MyOptionNullableFieldWithDefault._

    emptyArray.failsWith[MyClass](JsonTypeMismatch(_, JObject))
    emptyObject.succeedsWith(MyClass(None))
    stringType.failsWith[MyClass](JsonTypeMismatch(_, JNumber, JNull))
    nullField.succeedsWith(MyClass(Some(Null)))
    intField.succeedsWith(MyClass(Some(NotNull(-17))))
  }

  it("should heed the configuration for field names") {
    import FieldDecoderTest.MyBareField.MyClass
    implicit val decoder: JObjectDecoder[MyClass] =
      deriveDecoderForCaseClass[MyClass](Config(fieldNameMapping = CaseTransformation(CamelCase, PascalCase)))

    json"""{"A": -13437}""".asRootFocus.decode[MyClass].shouldSucceed shouldBe MyClass(-13437)
  }
}
