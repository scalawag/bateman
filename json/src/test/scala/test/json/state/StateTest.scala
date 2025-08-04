// bateman -- Copyright 2021-2026 -- Justin Patterson
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

package test.json.state

import cats.syntax.either._
import org.scalawag.bateman.json.{focus => _, _}
import org.scalawag.bateman.json.focus.{JChildFocus, JFocus, JRootFocus}
import org.scalawag.bateman.json.lens.{focus => _, root => _, _}
import org.scalawag.bateman.json.state._
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

  it("should return the focus") {
    def s =
      for {
        x <- focus[JAny]
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
        x <- root[JAny]
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
          f <- apply("c" ~> 0 ~> narrowTo[JNumber])
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
      _ <- replace(true)
    } yield ()

    e.runS(json).shouldSucceed.root.value shouldEncodeTo
      parse("""{"a":{"g":4,"f":true,"b":null},"b":6,"c":[11,12,13]}""").value
  }

  it("should narrow the focus to a specific type") {
    val e = for {
      _ <- down("a")
      _ <- narrow[JObject]
    } yield ()

    val result = e.runS(json).shouldSucceed
    result.value shouldBe an[JObject]
    result.value.fieldList.map(_.name.value) should contain("g")
  }

  it("should fail to narrow the focus to a wrong type") {
    val e = for {
      _ <- down("b")
      _ <- narrow[JString]
    } yield ()

    e.runS(json).shouldFailSingle shouldBe a[JsonTypeMismatch]
  }

  it("should delete the focus") {
    val e = for {
      _ <- down("a")
      _ <- down("b")
      _ <- delete()
    } yield ()

    e.runS(json).shouldSucceed.root.value shouldEncodeTo
      parse("""{"a":{"g":4,"f":"thing"},"b":6,"c":[11,12,13]}""").value
  }

  it("should encode deeply") {
    val e = for {
      _ <- down("a")
      b <- encodeTo("x" ~> "y", 89)
    } yield b

    e.runS(json).shouldSucceed.root.value shouldEncodeTo
      parse("""{"a":{"g":4,"f":"thing","b":true,"x":{"y":89}},"b":6,"c":[11,12,13]}""").value
  }

  it("should modify the focus (pure, focus-based)") {
    def fn(f: JFocus[JNumber]) = f.value.toBigDecimal * 2

    val e = for {
      _ <- down("a")
      _ <- down("g")
      _ <- narrow[JNumber]
      _ <- modifyFocus(fn)
    } yield ()

    val result = e.runS(json).shouldSucceed
    result.root.value shouldEncodeTo
      parse("""{"a":{"g":8,"f":"thing","b":true},"b":6,"c":[11,12,13]}""").value
  }

  it("should modify the focus (pure, value-based)") {
    def fn(n: JAny) = "replaced"

    val e = for {
      _ <- down("a")
      _ <- down("g")
      _ <- modify(fn)
    } yield ()

    val result = e.runS(json).shouldSucceed
    result.root.value shouldEncodeTo
      parse("""{"a":{"g":"replaced","f":"thing","b":true},"b":6,"c":[11,12,13]}""").value
  }

  it("should modify the focus (fallible, focus-based)") {
    def fn(f: JFocus[JNumber]) = f.decode[Int].map(_ * 2)

    val e = for {
      _ <- down("a")
      _ <- down("g")
      _ <- narrow[JNumber]
      _ <- modifyFocusF(fn)
    } yield ()

    val result = e.runS(json).shouldSucceed
    result.root.value shouldEncodeTo
      parse("""{"a":{"g":8,"f":"thing","b":true},"b":6,"c":[11,12,13]}""").value
  }

  it("should modify the focus (fallible, value-based)") {
    // JSON numbers are strings internally, so this means double the string.
    def fn(n: JNumber) = (n.value * 2).rightNec[JError]

    val e = for {
      _ <- down("a")
      _ <- down("g")
      _ <- narrow[JNumber]
      _ <- modifyF(fn)
    } yield ()

    val result = e.runS(json).shouldSucceed
    result.root.value shouldEncodeTo
      parse("""{"a":{"g":"44","f":"thing","b":true},"b":6,"c":[11,12,13]}""").value
  }

  it("should modify the focus and propagate errors") {
    def fn(f: JFocus[JAny]) = JsonTypeMismatch(f, JNumber).leftNec[JNumber]

    val e = for {
      _ <- down("a")
      _ <- down("f")
      _ <- modifyFocusF(fn)
    } yield ()

    e.runS(json).shouldFailSingle shouldBe a[JsonTypeMismatch]
  }

  it("should encode a value into the focus") {
    val e = for {
      _ <- down("a")
      _ <- down("g")
      _ <- encode(42)
    } yield ()

    e.runS(json).shouldSucceed.root.value shouldEncodeTo
      parse("""{"a":{"g":42,"f":"thing","b":true},"b":6,"c":[11,12,13]}""").value
  }

  it("should encodeTo with Some value") {
    val e = encodeTo("x", Some(89))

    val (focus, value) = e.run(json).shouldSucceed
    value shouldBe Some(JNumber(89))
    focus.root.value shouldEncodeTo
      parse("""{"a":{"g":4,"f":"thing","b":true},"b":6,"c":[11,12,13],"x":89}""").value
  }

  it("should encodeTo with None value") {
    val e = encodeTo("x", Option.empty[Int])

    val (focus, value) = e.run(json).shouldSucceed
    value shouldBe None
    focus.root.value shouldEncodeTo json.value
  }

  it("should decodeThrough with a cursor lens") {
    def s =
      for {
        x <- decodeThrough[BigDecimal]("c" ~> items ~> narrowTo[JNumber])
      } yield x

    val (out, value) = s.run(json).shouldSucceed
    out shouldBe json
    value shouldBe List(BigDecimal(11), BigDecimal(12), BigDecimal(13))
  }
}
