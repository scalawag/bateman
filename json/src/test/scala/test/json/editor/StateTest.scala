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

package test.json.editor

import org.scalawag.bateman.json._
import org.scalawag.bateman.json.focus.{JChildFocus, JFocus, JRootFocus}
import org.scalawag.bateman.json.lens._
import org.scalawag.bateman.json.state._
import org.scalawag.bateman.json.focus.weak._
import test.json.BatemanTestBase

class StateTest extends BatemanTestBase {
  private val json = parse("""
    {
      "a": {
        "g": 4,
        "f": "thing",
        "b": true
      },
      "b": 6,
      "c": [11, 12, 13]
    }
  """)

  private def jarray = json("c" ~> lens.narrow[JArray]).shouldSucceed

  it("should return the focus") {
    def s =
      for {
        x <- state.focus[JAny]
      } yield x

    forAll(genJFocus(genJAny)) { f =>
      val (out, value) = s.run(f).shouldSucceed
      out shouldBe f
      value shouldBe f
    }
  }

  it("should return the value") {
    def s =
      for {
        x <- state.value[JAny]
      } yield x

    forAll(genJFocus(genJAny)) { f =>
      val (out, value) = s.run(f).shouldSucceed
      out shouldBe f
      value shouldBe f.value
    }
  }

  it("should return the root") {
    def s =
      for {
        x <- state.root[JAny]
      } yield x

    forAll(genJFocus(genJAny)) { f =>
      val (out, value) = s.run(f).shouldSucceed
      out shouldBe f.root
      value shouldBe f.root.value
    }
  }

  describe("apply") {
    import org.scalawag.bateman.json.lens._

    it("should successfully apply a CreatableJLens") {
      val state =
        for {
          f <- apply("a" ~> "g")
        } yield f

      val (focus, value) = state.run(json).shouldSucceed

      focus.root shouldBe json
      value shouldBe focus.value
      focus.pointer.toString shouldBe "/a/g"
    }

    it("should successfully apply an IdJLens") {
      val state =
        for {
          f <- apply("c" ~> 0 ~> narrow[JNumber])
        } yield f

      val (focus, value) = state.run(json).shouldSucceed

      focus.root shouldBe json
      focus.pointer.toString shouldBe "/c/0"
      value.value shouldBe "11"
    }

    it("should fail to apply a lens") {
      val state =
        for {
          f <- apply("b" ~> "g")
        } yield f

      state.run(json).shouldFailSingle
      JsonTypeMismatch(json.asObject.flatMap(_.field("b")).shouldSucceed, JObject)
    }
  }

  describe("down(String)") {
    it("should descend to a child") {
      forAll(genJFocus(genNonEmptyJObject)) { f =>
        val name = f.value.fieldList.head.name.value
        val (out, value) = down(name).run(f).shouldSucceed
        out shouldBe f.field(name).shouldSucceed
        value shouldBe f.field(name).shouldSucceed.value
      }
    }

    it("should fail to descend to missing field") {
      forAll(genJFocus(genEmptyJObject)) { f =>
        down("name").run(f).shouldFailSingle shouldBe MissingField(f, "name")
      }
    }

    it("should fail to descend into not-an-object") {
      forAll(genJFocus(genEmptyJArray)) { f =>
        down("name").run(f).shouldFailSingle shouldBe JsonTypeMismatch(f, JObject)
      }
    }
  }

  describe("down(Int)") {
    it("should descend to an item") {
      forAll(genJFocus(genNonEmptyJArray)) { f =>
        val (out, value) = down(0).run(f).shouldSucceed
        out shouldBe f.items.head
        value shouldBe f.items.head.value
      }
    }

    it("should fail to descend to missing item") {
      forAll(genJFocus(genEmptyJArray)) { f =>
        down(99).run(f).shouldFailSingle shouldBe MissingIndex(f, 99)
      }
    }

    it("should fail to descend into not-an-array") {
      forAll(genJFocus(genEmptyJObject)) { f =>
        down(8).run(f).shouldFailSingle shouldBe JsonTypeMismatch(f, JArray)
      }
    }
  }

  describe("up") {
    it("should ascend to the parent") {
      forAll(genJFocus(genJAny)) {
        case f: JRootFocus[_] =>
          up.run(f).shouldFailSingle shouldBe NoParent(f)
        case f: JChildFocus[_, JFocus[JAny]] =>
          val (out, value) = up.run(f).shouldSucceed
          out shouldBe f.parent
          value shouldBe f.parent.value
      }
    }

    it("should fail to ascend above the root") {
      forAll(genJAny) { f =>
        up.run(f.asRootFocus).shouldFailSingle shouldBe NoParent(f.asRootFocus)
      }
    }
  }

  it("should decode the focus") {
    def s =
      for {
        x <- decode[String]
      } yield x

    forAll(genJFocus(genJString)) { f =>
      val (out, value) = s.run(f).shouldSucceed
      out shouldBe f
      value shouldBe f.value.value
    }
  }

  it("should decode through a lens") {
    import org.scalawag.bateman.json.lens._
    def s =
      for {
        x <- decodeThrough[String]("a" ~> "f")
      } yield x

    val (out, value) = s.run(json).shouldSucceed
    out shouldBe json
    value shouldBe "thing"
  }

  it("should edit things") {
    val e = for {
      _ <- down("a")
      _ <- down("b")
      _ <- replace(JNull)
      _ <- up
      _ <- down("f")
      _ <- replace(JBoolean(true))

    } yield ()

    e.runS(json).shouldSucceed.root.value shouldEncodeTo
      parse("""{"a":{"g":4,"f":true,"b":null},"b":6,"c":[11,12,13]}""").value
  }

  it("should delete the focus") {
    val e = for {
      _ <- down("a")
      _ <- down("b")
      _ <- delete
    } yield ()

    e.runS(json).shouldSucceed.root.value shouldEncodeTo
      parse("""{"a":{"g":4,"f":"thing"},"b":6,"c":[11,12,13]}""").value
  }

  it("should encode deeply") {
    val e = for {
      _ <- down("a")
      b <- encodeTo(lens.focus ~> "x" ~> "y", 89)
    } yield b

    e.runS(json).shouldSucceed.root.value shouldEncodeTo
      parse("""{"a":{"g":4,"f":"thing","b":true,"x":{"y":89}},"b":6,"c":[11,12,13]}""").value
  }
}
