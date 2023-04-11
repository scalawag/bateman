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

import org.scalawag.bateman.jsonapi.generic.Annotations.Id
import org.scalawag.bateman.json.literal._
import org.scalawag.bateman.json.syntax._
import test.json.BatemanTestBase
import HListIdEncoderTest._
import org.scalawag.bateman.json.JStringEncoder
import cats.syntax.contravariant._

object HListIdEncoderTest {
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

  object MyStringIdWithDefault {
    case class MyClass(@Id b: String = "xxx")
  }
}

class HListIdEncoderTest extends BatemanTestBase {
  describe("MyStringId") {
    import MyStringId._
    import org.scalawag.bateman.jsonapi.generic.auto._

    it("should encode") {
      MyClass("67").toJAny shouldEncodeTo json"""{"type": "MyClass", "id": "67"}"""
    }
  }

  describe("MyIntId") {
    import MyIntId._
    import org.scalawag.bateman.jsonapi.generic.auto._
    implicit val idEncoder: JStringEncoder[Int] = JStringEncoder[String].contramap(_.toString)

    it("should encode") {
      MyClass(67).toJAny shouldEncodeTo json"""{"type": "MyClass", "id": "67"}"""
    }
  }

  describe("MyOptionStringId") {
    import MyOptionStringId._
    import org.scalawag.bateman.jsonapi.generic.auto._

    it("should encode Some") {
      MyClass(Some("67")).toJAny shouldEncodeTo json"""{"type": "MyClass", "id": "67"}"""
    }

    it("should encode None") {
      MyClass(None).toJAny shouldEncodeTo json"""{"type": "MyClass"}"""
    }
  }

  describe("MyOptionIntId") {
    import MyOptionIntId._
    import org.scalawag.bateman.jsonapi.generic.auto._
    implicit val idEncoder: JStringEncoder[Int] = JStringEncoder[String].contramap(_.toString)

    it("should encode Some") {
      MyClass(Some(67)).toJAny shouldEncodeTo json"""{"type": "MyClass", "id": "67"}"""
    }

    it("should encode None") {
      MyClass(None).toJAny shouldEncodeTo json"""{"type": "MyClass"}"""
    }
  }

  describe("MyStringId") {
    import MyStringId._
    import org.scalawag.bateman.jsonapi.generic.auto._

    it("should encode default value") {
      MyClass("xxx").toJAny shouldEncodeTo json"""{"type": "MyClass", "id": "xxx"}"""
    }
  }
}
