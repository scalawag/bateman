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
import org.scalacheck.Gen
import org.scalawag.bateman.json._
import org.scalawag.bateman.json.focus._
import test.json.BatemanTestBase

class JFocusOpsTest extends BatemanTestBase {

  val genDeepFocus: Gen[JFieldFocus[JString, JRootFocus[JObject]]] =
    for {
      obj <- genJObject
      name <- genJString.map(_.value)
      value <- genJString
    } yield {
      val root = obj.append(name, value)
      root.asRootFocus.asObject.flatMap(x => x.field(name)).flatMap(_.asString).getOrThrow
    }

  val mutator: JString => JString = in => JString(in.value * 2)
  val fmutator: JFieldFocus[JString, JRootFocus[JObject]] => JString = mutator.compose(_.value)

  describe("modify") {
    it("should modify the focus") {
      forAll(genDeepFocus) { f =>
        val out = f.modify(fmutator)

        out.pointer shouldBe f.pointer
        out.value shouldBe fmutator(f)
        out.root.value.shouldHaveNoLocations
      }
    }
  }

  describe("modifyF") {
    it("should modify the focus") {
      forAll(genDeepFocus) { f =>
        val out = f.modifyF(fmutator.andThen(_.rightNec)).shouldSucceed

        out.pointer shouldBe f.pointer
        out.value shouldBe fmutator(f)
        out.root.value.shouldHaveNoLocations
      }
    }

    it("should fail to modify the focus") {
      forAll(genDeepFocus) { f =>
        val err = JsonTypeMismatch(f, JNull)
        val out = f.modifyF(_ => err.leftNec)

        out.shouldFailSingle shouldBe err
      }
    }
  }

  describe("modifyValue") {
    it("should modify the value in focus") {
      forAll(genDeepFocus) { f =>
        val out = f.modifyValue(mutator)

        out.pointer shouldBe f.pointer
        out.value shouldBe mutator(f.value)
        out.root.value.shouldHaveNoLocations
      }
    }
  }

  describe("modifyValueF") {
    it("should modify the value in focus") {
      forAll(genDeepFocus) { f =>
        val out = f.modifyValueF(mutator.andThen(_.rightNec)).shouldSucceed

        out.pointer shouldBe f.pointer
        out.value shouldBe mutator(f.value)
        out.root.value.shouldHaveNoLocations
      }
    }

    it("should fail to modify the value in focus") {
      forAll(genDeepFocus) { f =>
        val err = JsonTypeMismatch(f, JNull)
        val out = f.modifyValueF(_ => err.leftNec)

        out.shouldFailSingle shouldBe err
      }
    }
  }
}
