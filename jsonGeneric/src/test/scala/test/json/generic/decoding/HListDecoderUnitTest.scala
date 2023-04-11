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

import cats.data.NonEmptyChain
import org.scalawag.bateman.json._
import org.scalawag.bateman.json.focus.JFocus
import org.scalawag.bateman.json.generic.decoding.HListDecoderFactory.{Input, Output}
import org.scalawag.bateman.json.generic.decoding.{HListDecoderFactory, JSource}
import org.scalawag.bateman.json.generic.{CaseClassInfo, Config, Source}
import shapeless.{::, HNil}
import test.json.BatemanTestBase

/** These may be exceeding fragile and maybe should be deleted in favor of the more functional tests in
  * FieldDecoderTest and SourceFieldDecoderTest.
  */
class HListDecoderUnitTest extends BatemanTestBase {
  describe("hnilDecoder") {
    val decoderFactory = implicitly[HListDecoderFactory[HNil, HNil, HNil]]
    val strictConfig = Config(allowUnknownFields = false)
    val json = JObject("a" -> JNumber(1), "b" -> JBoolean(true)).asRootFocus
    val fa = json.field("a").getOrThrow
    val fb = json.field("b").getOrThrow

    it("should succeed") {
      decoderFactory(null, Config.default)
        .decode(Input(JObject.Empty.asRootFocus, Set.empty, Map.empty))
        .shouldSucceed shouldBe Output(HNil, Map.empty)
    }

    it("should ignore unused fields") {
      decoderFactory(null, Config.default)
        .decode(Input(json, Set.empty, Map.empty))
        .shouldSucceed shouldBe Output(HNil, Map.empty)
    }

    it("should reject extra JSON fields") {
      decoderFactory(null, strictConfig)
        .decode(Input(json, Set.empty, Map.empty))
        .shouldFail shouldBe NonEmptyChain(UnexpectedValue(fa), UnexpectedValue(fb))
    }

    it("should reject extra JSON fields that aren't used to decode HList fields") {
      decoderFactory(null, strictConfig)
        .decode(Input(json, Set.empty, Map("a" -> fa)))
        .shouldFailSingle shouldBe UnexpectedValue(fb)
    }

    it("should reject extra JSON fields that aren't used as discriminators") {
      decoderFactory(null, strictConfig)
        .decode(Input(json, Set(fa), Map.empty))
        .shouldFailSingle shouldBe UnexpectedValue(fb)
    }
  }

  describe("hconsDecoder") {
    val decoderFactory = implicitly[HListDecoderFactory[Int :: HNil, Option[Int] :: HNil, HNil :: HNil]]
    val cciWithDefault = CaseClassInfo(Option(4) :: HNil, List("a"))
    val cciWithoutDefault = CaseClassInfo((None: Option[Int]) :: HNil, List("a"))
    val finWithValue = JObject("a" -> JNumber(8)).asRootFocus
    val fa = finWithValue.field("a").getOrThrow
    val finWithoutValue = JObject.Empty.asRootFocus

    it("should decode successfully from input with no default") {
      decoderFactory(cciWithoutDefault, Config.default)
        .decode(Input(finWithValue, Set.empty, Map.empty))
        .shouldSucceed shouldBe
        Output(
          8 :: HNil,
          Map("a" -> fa)
        )
    }

    it("should decode successfully from default with no input") {
      decoderFactory(cciWithDefault, Config.default)
        .decode(Input(finWithoutValue, Set.empty, Map.empty))
        .shouldSucceed shouldBe
        Output(
          4 :: HNil,
          Map.empty
        )
    }

    it("should decode successfully and choose input over default") {
      decoderFactory(cciWithDefault, Config.default)
        .decode(Input(finWithValue, Set.empty, Map.empty))
        .shouldSucceed shouldBe
        Output(
          8 :: HNil,
          Map("a" -> fa)
        )
    }

    it("should fail with neither input nor default") {
      decoderFactory(cciWithoutDefault, Config.default)
        .decode(Input(finWithoutValue, Set.empty, Map.empty))
        .shouldFailSingle shouldBe MissingField(finWithoutValue, "a")
    }

    it("should fail when default is ignored") {
      decoderFactory(cciWithDefault, Config(useDefaultsForMissingFields = false))
        .decode(Input(finWithoutValue, Set.empty, Map.empty))
        .shouldFailSingle shouldBe MissingField(finWithoutValue, "a")
    }
  }

  describe("hconsOptionDecoder") {
    val decoderFactory = implicitly[HListDecoderFactory[Option[Int] :: HNil, Option[Option[Int]] :: HNil, HNil :: HNil]]
    val cciWithDeepDefault = CaseClassInfo(Option(Option(4)) :: HNil, List("a"))
    val cciWithDefault = CaseClassInfo(Option(None: Option[Int]) :: HNil, List("a"))
    val cciWithoutDefault = CaseClassInfo((None: Option[Option[Int]]) :: HNil, List("a"))
    val finWithValue = JObject("a" -> JNumber(8)).asRootFocus
    val fa = finWithValue.field("a").getOrThrow
    val finWithoutValue = JObject.Empty.asRootFocus

    it("should decode successfully from input with no default") {
      decoderFactory(cciWithoutDefault, Config.default)
        .decode(Input(finWithValue, Set.empty, Map.empty))
        .shouldSucceed shouldBe
        Output(
          Some(8) :: HNil,
          Map("a" -> fa)
        )
    }

    it("should decode successfully from deep default with no input") {
      decoderFactory(cciWithDeepDefault, Config.default)
        .decode(Input(finWithoutValue, Set.empty, Map.empty))
        .shouldSucceed shouldBe
        Output(
          None :: HNil,
          Map.empty
        )
    }

    it("should decode successfully from default with no input") {
      decoderFactory(cciWithDefault, Config.default)
        .decode(Input(finWithoutValue, Set.empty, Map.empty))
        .shouldSucceed shouldBe
        Output(
          None :: HNil,
          Map.empty
        )
    }

    it("should decode successfully and choose input over deep default") {
      decoderFactory(cciWithDeepDefault, Config.default)
        .decode(Input(finWithValue, Set.empty, Map.empty))
        .shouldSucceed shouldBe
        Output(
          Some(8) :: HNil,
          Map("a" -> fa)
        )
    }

    it("should decode successfully and choose input over default") {
      decoderFactory(cciWithDefault, Config.default)
        .decode(Input(finWithValue, Set.empty, Map.empty))
        .shouldSucceed shouldBe
        Output(
          Some(8) :: HNil,
          Map("a" -> fa)
        )
    }

    it("should succeed with neither input nor default") {
      decoderFactory(cciWithoutDefault, Config.default)
        .decode(Input(finWithoutValue, Set.empty, Map.empty))
        .shouldSucceed shouldBe
        Output(
          None :: HNil,
          Map.empty
        )
    }

    it("should succeed when deep default is ignored") {
      decoderFactory(cciWithDeepDefault, Config(useDefaultsForMissingFields = false))
        .decode(Input(finWithoutValue, Set.empty, Map.empty))
        .shouldSucceed shouldBe
        Output(
          None :: HNil,
          Map.empty
        )
    }

    it("should succeed when default is ignored") {
      decoderFactory(cciWithDefault, Config(useDefaultsForMissingFields = false))
        .decode(Input(finWithoutValue, Set.empty, Map.empty))
        .shouldSucceed shouldBe
        Output(
          None :: HNil,
          Map.empty
        )
    }
  }

  describe("hconsSourceDecoder") {
    val decoderFactory =
      implicitly[HListDecoderFactory[JSource :: HNil, Option[JSource] :: HNil, (Source :: HNil) :: HNil]]
    val cciWithDefault = CaseClassInfo(Option(JSource(JObject.Empty.asRootFocus, Map.empty)) :: HNil, List("a"))
    val cciWithoutDefault = CaseClassInfo((None: Option[JSource]) :: HNil, List("a"))
    val finWithValue = JObject("a" -> JNumber(8)).asRootFocus
    val finWithoutValue = JObject.Empty.asRootFocus

    // These should return the same thing every time and there's no way for it to fail

    def runTest(cci: CaseClassInfo[Option[JSource] :: HNil], fin: JFocus[JObject]) = {
      decoderFactory(cci, Config.default)
        .decode(Input(fin, Set.empty, Map.empty))
        .shouldSucceed shouldBe
        Output(
          JSource(fin) :: HNil,
          Map.empty
        )
    }

    it("should decode successfully") {
      runTest(cciWithoutDefault, finWithoutValue)
    }

    it("should decode successfully ignoring input") {
      runTest(cciWithoutDefault, finWithValue)
    }

    it("should decode successfully ignoring default") {
      runTest(cciWithDefault, finWithoutValue)
    }

    it("should decode successfully ignoring input and default") {
      runTest(cciWithDefault, finWithValue)
    }
  }

  describe("hconsOptionSourceDecoder") {
    type Head = Option[JSource]
    val decoderFactory = implicitly[HListDecoderFactory[Head :: HNil, Option[Head] :: HNil, (Source :: HNil) :: HNil]]
    val cciWithDeepDefault =
      CaseClassInfo(Option(Option(JSource(JObject.Empty.asRootFocus, Map.empty))) :: HNil, List("a"))
    val cciWithDefault = CaseClassInfo(Option(None: Head) :: HNil, List("a"))
    val cciWithoutDefault = CaseClassInfo((None: Option[Head]) :: HNil, List("a"))
    val finWithValue = JObject("a" -> JNumber(8)).asRootFocus
    val finWithoutValue = JObject.Empty.asRootFocus

    // These should return the same thing every time and there's no way for it to fail

    def runTest(cci: CaseClassInfo[Option[Option[JSource]] :: HNil], fin: JFocus[JObject]) = {
      decoderFactory(cci, Config.default)
        .decode(Input(fin, Set.empty, Map.empty))
        .shouldSucceed shouldBe
        Output(
          Some(JSource(fin)) :: HNil,
          Map.empty
        )
    }

    it("should decode successfully") {
      runTest(cciWithoutDefault, finWithoutValue)
    }

    it("should decode successfully ignoring input") {
      runTest(cciWithoutDefault, finWithValue)
    }

    it("should decode successfully ignoring default") {
      runTest(cciWithDefault, finWithoutValue)
    }

    it("should decode successfully ignoring input and default") {
      runTest(cciWithDefault, finWithValue)
    }

    it("should decode successfully ignoring deep default") {
      runTest(cciWithDeepDefault, finWithoutValue)
    }

    it("should decode successfully ignoring input and deep default") {
      runTest(cciWithDeepDefault, finWithValue)
    }
  }
}
