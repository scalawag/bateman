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
import org.scalawag.bateman.json.{JArray, JBoolean, JLocation, JNull, JNumber, JObject, JString}
import test.json.BatemanTestBase

class JArrayTest extends BatemanTestBase {
  private val i1 = JNumber.unsafe("4.37")
  private val i2 = JString("froboz")
  private val i3 = JNull
  private val i4 = JBoolean(false)

  describe("apply") {

    it("should create an empty array") {
      JArray().items shouldBe empty
    }

    it("should create an array of one item") {
      JArray(i1).items shouldBe List(i1)
    }

    it("should create an array of two items") {
      JArray(i1, i2).items shouldBe List(i1, i2)
    }

    it("should create an array of three items") {
      JArray(i1, i2, i3).items shouldBe List(i1, i2, i3)
    }

    it("should strip locations") {
      forAll(genJAny) { in =>
        JArray(in).shouldHaveNoLocations
      }
    }
  }

  describe("monoid") {
    it("should have empty") {
      Monoid[JArray].empty shouldBe JArray()
    }

    it("should combine") {
      Monoid[JArray].combine(JArray(i1, i2), JArray(i3, i4)) shouldBe JArray(i1, i2, i3, i4)
    }

    it("should combine by keeping left") {
      val o = JArray(i1, i2).copy(location = Some(JLocation(23, 47)))
      Monoid[JArray].combine(o, JArray.Empty) shouldBe o
    }

    it("should combine by keeping right") {
      val o = JArray(i1, i2).copy(location = Some(JLocation(23, 47)))
      Monoid[JArray].combine(JArray.Empty, o) shouldBe o
    }

    it("should combine by keeping left (discard location)") {
      val o = JArray(i1, i2).copy(location = Some(JLocation(23, 47)))
      Monoid[JArray].combine(o, JArray(Nil, Some(JLocation(2, 3)))) shouldBe o
    }

    it("should combine by keeping right (discard location)") {
      val o = JArray(i1, i2).copy(location = Some(JLocation(23, 47)))
      Monoid[JArray].combine(JArray(Nil, Some(JLocation(2, 3))), o) shouldBe o
    }

    implicit def eqForJArray: Eq[JObject] = Eq.fromUniversalEquals
    checkAll("should pass cats-law tests", cats.kernel.laws.discipline.MonoidTests[JObject].monoid)
  }

  describe("unapply") {
    it("should extract the empty list") {
      inside(JArray()) {
        case JArray.Value(s) => s shouldBe Nil
      }
    }
    it("should extract the single value") {
      inside(JArray(i1)) {
        case JArray.Value(s) => s shouldBe List(i1)
      }
    }
    it("should extract the values") {
      inside(JArray(i1, i2)) {
        case JArray.Value(s) => s shouldBe List(i1, i2)
      }
    }
  }

  describe("unapplySeq") {
    it("should extract just the values with no items") {
      inside(JArray()) {
        case JArray.Values() =>
      }
    }
    it("should extract just the values with one item") {
      inside(JArray(i1)) {
        case JArray.Values(m1) => m1 shouldBe i1
      }
    }
    it("should extract just the values with two items") {
      inside(JArray(i1, i2)) {
        case JArray.Values(m1, m2) => m1 shouldBe i1; m2 shouldBe i2
      }
    }
  }

  it("length") {
    forAll(genJArray) { in =>
      in.length shouldBe in.items.length
    }
  }

  it("updated") {
    forAll(genJArray, genJAny) { (in, value) =>
      in.items.indices.foreach { n =>
        val out = in.updated(n, value)
        out.items shouldBe in.items.updated(n, value).map(_.stripLocation)
        out.shouldHaveNoLocations
      }
    }
  }

  it("delete") {
    forAll(genJArray) { in =>
      in.items.indices.foreach { n =>
        val out = in.delete(n)
        out.length shouldBe in.length - 1
        out.items shouldNot contain(in.items(n))
        out.shouldHaveNoLocations
      }
    }
  }

  it("insert") {
    forAll(genJArray, genJAny) { (in, item) =>
      (0 to in.length).foreach { n =>
        val out = in.insert(n, item)
        out.length shouldBe in.length + 1
        out.items(n) shouldBe item.stripLocation
        out.shouldHaveNoLocations
      }
    }
  }

  it("prepend") {
    forAll(genJArray, genJAny) { (in, item) =>
      val out = in.prepend(item)
      out.length shouldBe in.length + 1
      out.items.head shouldBe item.stripLocation
      out.items.tail shouldBe in.items.map(_.stripLocation)
      out.shouldHaveNoLocations
    }
  }

  it("append") {
    forAll(genJArray, genJAny) { (in, item) =>
      val out = in.append(item)
      out.length shouldBe in.length + 1
      out.items.last shouldBe item.stripLocation
      out.items.init shouldBe in.items.map(_.stripLocation)
      out.shouldHaveNoLocations
    }
  }

  it("++") {
    forAll(genJArray, genJArray) { (in1, in2) =>
      val out = in1 ++ in2
      out.length shouldBe in1.length + in2.length
      out.items shouldBe in1.items.map(_.stripLocation) ::: in2.items.map(_.stripLocation)
      out.shouldHaveNoLocations
    }
  }

}
