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

package test.json.focus

import cats.syntax.either._
import org.scalawag.bateman.json.JType.Summoner
import org.scalawag.bateman.json._
import org.scalawag.bateman.json.focus._
import org.scalawag.bateman.json.focus.weak._
import test.json.BatemanTestBase

import scala.reflect.ClassTag

class JFocusJAnyWeakOpsTest extends BatemanTestBase {

  describe("narrowing") {
    val ins = List(JString("a"), JNumber(4), JBoolean(true), JNull, JObject.Empty, JArray.Empty).map(_.asRootFocus)

    def narrowTestCases[A <: JAny: ClassTag: Summoner](): Unit = {
      describe(s"narrow[${JType[A]}]") {
        ins.foreach { in =>
          if (in.value.jType == JType[A])
            it(s"should succeed from ${in.value.jType}") {
              val out: JResult[JFocus[A]] = in.narrow[A]
              out shouldBe in.rightNec
            }
          else
            it(s"should fail from ${in.value.jType}") {
              val out: JResult[JFocus[A]] = in.narrow[A]
              out shouldBe JsonTypeMismatch(in, JType[A]).leftNec
            }
        }
      }
    }

    narrowTestCases[JNull]()
    narrowTestCases[JNumber]()
    narrowTestCases[JString]()
    narrowTestCases[JBoolean]()
    narrowTestCases[JObject]()
    narrowTestCases[JArray]()

    def asTestCases[A <: JAny: Summoner](fn: JFocus[JAny] => JResult[JFocus[A]]): Unit = {
      describe(s"as${JType[A].toString.tail}") {
        ins.foreach { in =>
          if (in.value.jType == JType[A])
            it(s"should succeed from ${in.value.jType}") {
              val out: JResult[JFocus[A]] = fn(in)
              out shouldBe in.rightNec
            }
          else
            it(s"should fail from ${in.value.jType}") {
              val out: JResult[JFocus[A]] = fn(in)
              out shouldBe JsonTypeMismatch(in, JType[A]).leftNec
            }
        }
      }
    }

    asTestCases(_.asNull)
    asTestCases(_.asNumber)
    asTestCases(_.asString)
    asTestCases(_.asBoolean)
    asTestCases(_.asObject)
    asTestCases(_.asArray)
  }

  val janyMutator: JAny => JAny = {
    case _: JNull    => JNumber(-1)
    case s: JString  => JNumber(s.value.length)
    case b: JBoolean => if (b.value) JNumber(1001) else JNumber(1000)
    case n: JNumber  => JNumber(n.toBigDecimal.toInt * 17)
    case a: JArray   => a.prepend(JBoolean(false))
    case o: JObject  => o.append("modified", JBoolean(true))
  }

  val focusMutator: JFocus[JAny] => JAny = janyMutator.compose(_.value)

  describe("replace") {
    it("should replace the value in focus") {
      forAll(genJFocus(genJAny), genJAny) { (in, value) =>
        val out = in.replace(value)

        out.pointer shouldBe in.pointer
        out.value shouldBe value.stripLocation
        out.root.value.shouldHaveNoLocations

        inside(out.parentOption, in.parentOption) {
          case (Some(JFocus.Value(o: JObject)), Some(JFocus.Value(i: JObject))) =>
            val index = in.asInstanceOf[JFieldFocus[_, _]].index
            o shouldBe i.updated(index, value)
          case (Some(JFocus.Value(o: JArray)), Some(JFocus.Value(i: JArray))) =>
            val index = in.asInstanceOf[JItemFocus[_, _]].index
            o shouldBe i.updated(index, value)
          case (None, None) =>
            succeed
        }
      }
    }

    it("should return a narrow type (compilation)") {
      JNumber(4).asRootFocus.replace(JString("s")): JFocus[JString]
    }
  }

  describe("modify") {
    it("should modify the value in focus") {
      forAll(genJFocus(genJAny)) { f =>
        val modified = f.modify(focusMutator)

        modified.pointer shouldBe f.pointer
        modified.value shouldBe janyMutator(f.value)
        modified.root.value.shouldHaveNoLocations
      }
    }

    it("should return a narrow type (compilation)") {
      def fn: JFocus[JAny] => JString = ???
      def out: JFocus[JString] = JNumber(4).asRootFocus.modify(fn)
    }
  }

  describe("modifyF") {
    it("should modify the value in focus") {
      forAll(genJFocus(genJAny)) { f =>
        val successfulMutator = focusMutator.andThen(_.rightNec)
        val modified = f.modifyF(successfulMutator).shouldSucceed

        modified.pointer shouldBe f.pointer
        modified.value shouldBe janyMutator(f.value)
        modified.root.value.shouldHaveNoLocations
      }
    }

    it("should fail to modify the value in focus") {
      forAll(genJFocus(genJAny)) { f =>
        val modified = f.modifyF(JsonTypeMismatch(_, JNull).leftNec)

        modified shouldBe JsonTypeMismatch(f, JNull).leftNec
      }
    }

    it("should return a narrow type (compilation)") {
      def fn: JFocus[JAny] => JResult[JString] = ???
      def out: JResult[JFocus[JString]] = JNumber(4).asRootFocus.modifyF(fn)
    }
  }

  describe("modifyValue") {
    it("should modify the value in focus") {
      forAll(genJFocus(genJAny)) { f =>
        val modified = f.modifyValue(janyMutator)

        modified.pointer shouldBe f.pointer
        modified.value shouldBe janyMutator(f.value)
        modified.root.value.shouldHaveNoLocations
      }
    }

    it("should return a narrow type (compilation)") {
      def fn: JAny => JString = ???
      def out: JFocus[JString] = JNumber(4).asRootFocus.modifyValue(fn)
    }
  }

  describe("modifyValueF") {
    it("should modify the field at the focus") {
      forAll(genJFocus(genJAny)) { f =>
        val successfulMutator = janyMutator.andThen(_.rightNec)
        val modified = f.modifyValueF(successfulMutator).shouldSucceed

        modified.pointer shouldBe f.pointer
        modified.value shouldBe janyMutator(f.value)
        modified.root.value.shouldHaveNoLocations
      }
    }

    it("should fail to modify the field at the focus") {
      forAll(genJFocus(genJAny)) { f =>
        val modified = f.modifyValueF(f => JsonTypeMismatch(f.asRootFocus, JNull).leftNec)

        modified shouldBe JsonTypeMismatch(f.value.asRootFocus, JNull).leftNec
      }
    }

    it("should return a narrow type (compilation)") {
      def fn: JAny => JResult[JString] = ???
      def out: JResult[JFocus[JString]] = JNumber(4).asRootFocus.modifyValueF(fn)
    }
  }

  describe("delete") {
    it("should delete the value in focus") {
      forAll(genJFocus(genJAny).retryUntil(_.parentOption.isDefined)) { in =>
        val out = in.delete().shouldSucceed

        out.pointer shouldBe in.pointer.parent
        out.root.value.shouldHaveNoLocations

        inside((out.value, in.parentOption)) {
          case (o: JObject, Some(JFocus.Value(i: JObject))) =>
            val index = in.asInstanceOf[JFieldFocus[_, _]].index
            o shouldBe i.delete(index)
          case (o: JArray, Some(JFocus.Value(i: JArray))) =>
            val index = in.asInstanceOf[JItemFocus[_, _]].index
            o shouldBe i.delete(index)
        }
      }
    }

    it("should fail for root foci") {
      forAll(genJAny) { in =>
        in.asRootFocus.delete() shouldBe NoParent(in.asRootFocus).leftNec
      }
    }
  }

  describe("decode") {
    it("should decode the value") {
      forAll(genJFocus(genJString)) { in =>
        val out = in.decode[String].shouldSucceed

        out shouldBe in.value.value
      }
    }

    it("should fail of the decoder fails") {
      forAll(genJFocus(genJAny)) {
        case in @ JFocus.Value(s: JString) =>
          in.decode[String].shouldSucceed shouldBe s.value
        case in =>
          in.decode[String] shouldBe JsonTypeMismatch(in, JString).leftNec
      }
    }

    it("should decode the value with explicit decoder (use case)") {
      forAll(genJFocus(genJString)) { in =>
        val out = in.decode(Decoder.stringDecoder).shouldSucceed
        out shouldBe in.value.value
      }
    }
  }

  // TODO: create
  // TODO: navigate (drop)?
}
