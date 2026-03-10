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

package test.json.generic.decoding

import org.scalawag.bateman.json.generic.Config
import org.scalawag.bateman.json.literal._
import org.scalawag.bateman.json.{JObject, JObjectDecoder, UnexpectedValue}
import test.json.BatemanTestBase

object EmptyCaseClassTest {
  final case class MyClass()
}

class EmptyCaseClassTest extends BatemanTestBase {
  import EmptyCaseClassTest._

  describe("semiauto") {
    import org.scalawag.bateman.json.generic.semiauto._

    val f = json"""
      {
        "a": null
      }
    """.asRootFocus

    val fa = f.field("a").shouldSucceed

    it("should decode empty object") {
      implicit val decoder: JObjectDecoder[MyClass] = deriveDecoderForCaseClass[MyClass]()
      JObject.Empty.asRootFocus.decode[MyClass].shouldSucceed shouldBe MyClass()
    }

    it("should ignore extra field") {
      implicit val decoder: JObjectDecoder[MyClass] = deriveDecoderForCaseClass[MyClass]()

      f.decode[MyClass].shouldSucceed shouldBe MyClass()
    }

    it("should fail with extra field") {
      implicit val config: Config = Config(allowUnknownFields = false)
      implicit val decoder: JObjectDecoder[MyClass] = deriveDecoderForCaseClass[MyClass]()

      f.decode[MyClass].shouldFailSingle shouldBe UnexpectedValue(fa)
    }

    it("should allow extra field if it's a discriminator") {
      implicit val config: Config = Config(allowUnknownFields = false)
      val decoder: JObjectDecoder[MyClass] = deriveDecoderForCaseClass[MyClass]()

      decoder.decode(f, Set(fa)).shouldSucceed shouldBe MyClass()
    }
  }
}
