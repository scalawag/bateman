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

import org.scalawag.bateman.json.generic.decoding.JSource
import org.scalawag.bateman.json.generic.semiauto.unchecked._
import org.scalawag.bateman.json.generic.{Config, Source}
import org.scalawag.bateman.json.{JObject, JObjectDecoder}
import test.json.BatemanTestBase

object JSourceUseCasesTest {
  sealed trait X {
    val a: Int
    val src: Option[JSource]
  }

  object X {
    implicit val decoder: JObjectDecoder[X] = deriveDecoderForTrait[X]()
  }

  final case class Y(
      a: Int,
      b: String,
      @Source src: Option[JSource] = None
  ) extends X

  object Y {
    implicit val decoder: JObjectDecoder[Y] = deriveDecoderForCaseClass[Y]()
  }

  final case class Z(
      a: Int,
      c: Boolean,
      @Source src: Option[JSource] = None
  ) extends X

  object Z {
    implicit val decoder: JObjectDecoder[Z] = deriveDecoderForCaseClass[Z]()
  }

  final case class XX(x: X, @Source src: Option[JSource] = None)

  object XX {
    implicit val decoder: JObjectDecoder[XX] = deriveDecoderForCaseClass[XX]()
  }
}

class JSourceUseCasesTest extends BatemanTestBase {
  import JSourceUseCasesTest._

  it("should work for case classes") {
    val json = parseAs[JObject]("""
      {
        "a": 31,
        "b": "foo"
      }
    """)

    JObjectDecoder[Y].decode(json).shouldSucceed.src shouldBe
      Some(
        JSource(
          json,
          Map(
            "a" -> json.field("a").shouldSucceed,
            "b" -> json.field("b").shouldSucceed
          )
        )
      )
  }

  it("should work for traits") {
    val json = parseAs[JObject]("""
      {
        "type": "Y",
        "a": 31,
        "b": "foo"
      }
    """)

    JObjectDecoder[X].decode(json).shouldSucceed.src shouldBe
      Some(
        JSource(
          json,
          Map(
            "a" -> json.field("a").shouldSucceed,
            "b" -> json.field("b").shouldSucceed
          )
        )
      )
  }

  it("should work for nested fields") {
    val json = parseAs[JObject]("""
      {
        "x": {
          "type": "Y",
          "a": 31,
          "b": "foo"
        }
      }
    """)

    val xx = JObjectDecoder[XX].decode(json).shouldSucceed

    xx.src shouldBe
      Some(
        JSource(
          json,
          Map(
            "x" -> json.field("x").shouldSucceed
          )
        )
      )

    val x = json.field("x").shouldSucceed.asObject.shouldSucceed
    xx.x.src shouldBe
      Some(
        JSource(
          x,
          Map(
            "a" -> x.field("a").shouldSucceed,
            "b" -> x.field("b").shouldSucceed
          )
        )
      )
  }

  it("should use case transformations") {
    val json = parseAs[JObject]("""
      {
        "A": 31,
        "B": "foo"
      }
    """)

    implicit val config: Config = Config.default.copy(fieldNameMapping = _.toUpperCase)
    implicit val decoder: JObjectDecoder[Y] = deriveDecoderForCaseClass[Y]()
    JObjectDecoder[Y].decode(json).shouldSucceed.src shouldBe
      Some(
        JSource(
          json,
          Map(
            "a" -> json.field("A").shouldSucceed,
            "b" -> json.field("B").shouldSucceed
          )
        )
      )
  }
}
