// bateman -- Copyright 2021 -- Justin Patterson
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

package org.scalawag.bateman.json.generic.decoding

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import org.scalawag.bateman.json.ParserTestUtils
import org.scalawag.bateman.json.decoding.ContextualDecoder.ContextFreeDecoderOps
import org.scalawag.bateman.json.decoding.{JObject, JObjectDecoder, JPointer}
import org.scalawag.bateman.json.generic.{Config, SourceTag, semiauto}
import shapeless.tag.@@

import java.util.UUID

object JSourceTest {
  sealed trait X {
    val a: Int
    val src: Option[JSource]
  }

  object X {
    implicit val decoder: TraitDecoder[X, Any] = semiauto.deriveDecoderForTrait[X, Any]()
  }

  final case class Y(
      a: Int,
      b: String,
      src: Option[JSource] @@ SourceTag = None
  ) extends X

  object Y {
    implicit val decoder: CaseClassDecoder[Y, Any] = semiauto.deriveDecoderForCaseClass[Y, Any]()
  }

  final case class Z(
      a: Int,
      c: Boolean,
      src: Option[JSource] @@ SourceTag = None
  ) extends X

  object Z {
    implicit val decoder: CaseClassDecoder[Z, Any] = semiauto.deriveDecoderForCaseClass[Z, Any]()
  }

  final case class XX(x: X, src: Option[JSource] @@ SourceTag = None)

  object XX {
    implicit val decoder: CaseClassDecoder[XX, Any] = semiauto.deriveDecoderForCaseClass[XX, Any]()
  }
}

class JSourceTest extends AnyFunSpec with Matchers with ParserTestUtils {
  import JSourceTest._

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
            "a" -> JPointer.Root / "a",
            "b" -> JPointer.Root / "b",
            "src" -> JPointer.Root
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
            "a" -> JPointer.Root / "a",
            "b" -> JPointer.Root / "b",
            "src" -> JPointer.Root
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
            "x" -> JPointer.Root / "x",
            "src" -> JPointer.Root
          )
        )
      )

    xx.x.src shouldBe
      Some(
        JSource(
          json.fields.getOrElse(fail)("x").value.asObject.getOrElse(fail),
          Map(
            "a" -> JPointer.Root / "a",
            "b" -> JPointer.Root / "b",
            "src" -> JPointer.Root
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
    implicit val decoder: CaseClassDecoder[Y, Any] = semiauto.deriveDecoderForCaseClass[Y, Any]()
    JObjectDecoder[Y].decode(json).shouldSucceed.src shouldBe
      Some(
        JSource(
          json,
          Map(
            "a" -> JPointer.Root / "A",
            "b" -> JPointer.Root / "B",
            "src" -> JPointer.Root
          )
        )
      )
  }
}
