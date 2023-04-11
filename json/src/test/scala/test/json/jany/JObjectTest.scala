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

package test.json.jany

import cats.kernel.{Eq, Monoid}
import test.json.BatemanTestBase
import org.scalawag.bateman.json.{JBoolean, JField, JLocation, JNull, JNumber, JObject, JString}

class JObjectTest extends BatemanTestBase {
  private val i1 = JNumber.unsafe("4.37")
  private val i2 = JString("froboz")
  private val i3 = JNull
  private val i4 = JBoolean(false)

  describe("apply") {

    it("should create an empty object") {
      JObject().fieldList shouldBe empty
    }

    it("should create an object of one item") {
      JObject("a" -> i1).fieldList shouldBe List(JField(JString("a"), i1))
    }

    it("should create an object of two fieldList") {
      JObject("a" -> i1, "b" -> i2).fieldList shouldBe List(JField(JString("a"), i1), JField(JString("b"), i2))
    }
  }

  describe("flatten") {
    it("should create an object out of multiple iterables") {
      JObject.flatten(
        Some("a" -> i1),
        Iterable("b" -> i2),
        List("c" -> i3, "d" -> i4)
      ) shouldBe JObject("a" -> i1, "b" -> i2, "c" -> i3, "d" -> i4)
    }
  }

  describe("monoid") {
    it("should have empty") {
      Monoid[JObject].empty shouldBe JObject()
    }

    it("should combine") {
      val l = JObject("a" -> i1, "b" -> i2)
      val r = JObject("c" -> i3, "d" -> i4)
      Monoid[JObject].combine(l, r) shouldBe JObject("a" -> i1, "b" -> i2, "c" -> i3, "d" -> i4)
    }

    it("should combine by keeping left") {
      val o = JObject("a" -> i1, "b" -> i2).copy(location = Some(JLocation(17, 5)))
      Monoid[JObject].combine(o, JObject.Empty) shouldBe o
    }

    it("should combine by keeping right") {
      val o = JObject("a" -> i1, "b" -> i2).copy(location = Some(JLocation(17, 5)))
      Monoid[JObject].combine(JObject.Empty, o) shouldBe o
    }

    it("should combine by keeping left (discard location)") {
      val o = JObject("a" -> i1, "b" -> i2).copy(location = Some(JLocation(17, 5)))
      Monoid[JObject].combine(o, JObject(Nil, Some(JLocation(2, 3)))) shouldBe o
    }

    it("should combine by keeping right (discard location)") {
      val o = JObject("a" -> i1, "b" -> i2).copy(location = Some(JLocation(17, 5)))
      Monoid[JObject].combine(JObject(Nil, Some(JLocation(2, 3))), o) shouldBe o
    }

    implicit def eqJObject: Eq[JObject] = Eq.fromUniversalEquals
    checkAll("should pass cats-law tests", cats.kernel.laws.discipline.MonoidTests[JObject].monoid)
  }

  it("delete") {
    forAll(genJObject) { in =>
      in.fieldList.indices.foreach { n =>
        val out = in.delete(n)
        out.fieldList.length shouldBe in.fieldList.length - 1
        out.fieldList shouldNot contain(in.fieldList(n))
        out.shouldHaveNoLocations
      }
    }
  }

  it("updated") {
    forAll(genJObject, genJAny) { (in, value) =>
      in.fieldList.indices.foreach { n =>
        val out = in.updated(n, value)
        val field = JField(in.fieldList(n).name, value)
        out.fieldList shouldBe in.fieldList.updated(n, field).map(_.stripLocation)
        out.shouldHaveNoLocations
      }
    }
  }

  it("insert") {
    forAll(genJObject, genJString, genJAny) { (in, name, value) =>
      (0 to in.fieldList.length).foreach { n =>
        val out = in.insert(n, name.value, value)
        out.fieldList.length shouldBe in.fieldList.length + 1
        out.fieldList(n) shouldBe JField(name, value).stripLocation
        out.shouldHaveNoLocations
      }
    }
  }

  it("prepend") {
    forAll(genJObject, genJString, genJAny) { (in, name, value) =>
      val out = in.prepend(name.value, value)
      out.fieldList.length shouldBe in.fieldList.length + 1
      out.fieldList.head shouldBe JField(name, value).stripLocation
      out.fieldList.tail shouldBe in.fieldList.map(_.stripLocation)
      out.shouldHaveNoLocations
    }
  }

  it("append") {
    forAll(genJObject, genJString, genJAny) { (in, name, value) =>
      val out = in.append(name.value, value)
      out.fieldList.length shouldBe in.fieldList.length + 1
      out.fieldList.last shouldBe JField(name, value).stripLocation
      out.fieldList.init shouldBe in.fieldList.map(_.stripLocation)
      out.shouldHaveNoLocations
    }
  }

  it("++") {
    forAll(genJObject, genJObject) { (in1, in2) =>
      val out = in1 ++ in2
      out.fieldList.length shouldBe in1.fieldList.length + in2.fieldList.length
      out.fieldList shouldBe in1.fieldList.map(_.stripLocation) ::: in2.fieldList.map(_.stripLocation)
      out.shouldHaveNoLocations
    }
  }

}
