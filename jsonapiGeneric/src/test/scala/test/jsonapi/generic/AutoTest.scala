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

package test.jsonapi.generic

import org.scalawag.bateman.jsonapi.generic.Annotations.{Attribute, Id, IncludedRelationship}
import org.scalawag.bateman.jsonapi.generic.auto._
import org.scalawag.bateman.jsonapi.syntax._
import org.scalawag.bateman.json.lens._
import org.scalawag.bateman.jsonapi.lens._
import org.scalawag.bateman.json.literal._
import test.json.BatemanTestBase
import AutoTest._
import org.scalawag.bateman.json.JNumber
import org.scalawag.bateman.json.generic.Config
import org.scalawag.bateman.json.generic.naming.{CamelCase, KebabCase, PascalCase, SnakeCase}
import org.scalawag.bateman.jsonapi.generic.encoding.LidGenerator

import java.util.UUID

object AutoTest {
  final case class MyRef(@Attribute refFieldName: Int)
  final case class MyClass(@Id id: String, @Attribute a: Int = 8, @IncludedRelationship myRef: MyRef)
}

class AutoTest extends BatemanTestBase {
  it("should work by default") {
    val enc = MyClass("A", 8, MyRef(10)).toDocument.asRootFocus
    enc(data ~> relationships).shouldSucceed.value.fieldList.map(_.name.value) shouldBe List("myRef")
    enc(data ~> attributes.?).shouldSucceed.foci shouldBe None
    UUID.fromString(enc(included ~> 0 ~> lid).shouldSucceed.value.value) // should not throw
  }

  it("should use field name config") {
    implicit val cfg: Config = Config(fieldNameMapping = CamelCase to SnakeCase)
    val enc = MyClass("A", 8, MyRef(10)).toDocument.asRootFocus
    enc(data ~> resourceType).shouldSucceed.value.value shouldBe "MyClass"
    enc(data ~> relationships).shouldSucceed.value.fieldList.map(_.name.value) shouldBe List("my_ref")
    enc(included ~> 0 ~> resourceType).shouldSucceed.value.value shouldBe "MyRef"
    enc(included ~> 0 ~> attributes).shouldSucceed.value.fieldList.map(_.name.value) shouldBe List("ref_field_name")
  }

  it("should use class name config") {
    implicit val cfg: Config = Config(classNameMapping = PascalCase to KebabCase)
    val enc = MyClass("A", 8, MyRef(10)).toDocument.asRootFocus
    enc(data ~> resourceType).shouldSucceed.value.value shouldBe "my-class"
    enc(data ~> relationships).shouldSucceed.value.fieldList.map(_.name.value) shouldBe List("myRef")
    enc(included ~> 0 ~> resourceType).shouldSucceed.value.value shouldBe "my-ref"
    enc(included ~> 0 ~> attributes).shouldSucceed.value.fieldList.map(_.name.value) shouldBe List("refFieldName")
  }

  it("should encode default values") {
    implicit val cfg: Config = Config(encodeDefaultValues = true)
    val enc = MyClass("A", 8, MyRef(10)).toDocument.asRootFocus
    enc(data ~> attribute("a") ~> narrow[JNumber]).shouldSucceed.value.value shouldBe "8"
  }

  it("should use the LID generator") {
    implicit val lidgen: LidGenerator = () => "xxx"

    val enc = MyClass("A", 8, MyRef(10)).toDocument.asRootFocus
    println(enc.value.render)
    enc(data ~> relationship("myRef") ~> data ~> lid).shouldSucceed.value.value shouldBe "xxx"
    enc(included ~> 0 ~> lid).shouldSucceed.value.value shouldBe "xxx"
  }
}
