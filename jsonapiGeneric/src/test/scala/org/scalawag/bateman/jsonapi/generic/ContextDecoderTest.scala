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

package org.scalawag.bateman.jsonapi.generic

import cats.syntax.validated._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import org.scalawag.bateman.json.decoding.{ContextualDecoder, Decoder, InvalidValue, JAny, JString, JPointer, JLocation}
import org.scalawag.bateman.json.decoding.query._

class ContextDecoderTest extends AnyFunSpec with Matchers {
  it("should be required to pass the right contexts to a decoder") {
    final case class MyClass(name: String)

    def err(j: JAny) = InvalidValue(j, "input string was too long according to context").invalidNec

    implicit val decoder: ContextualDecoder[JString, MyClass, Int] = { (in, context) =>
      if (in.value.length < context)
        MyClass(in.value).validNec
      else
        err(in)
    }

    val a = JString("foobar", JLocation(1, 1), JPointer.Root)
    val q = root[JAny, Int] ~> as[MyClass]

    q(a, 12) shouldBe MyClass("foobar").validNec
    q(a, 4) shouldBe err(a)
  }

  it("should be able to use an Any context decoder with any context (duh)") {
    final case class MyClass(name: String)

    implicit val decoder: Decoder[JString, MyClass] = Decoder { in => MyClass(in.value).validNec }

    val a = JString("foobar", JLocation(1, 1), JPointer.Root)
    val q = root[JAny, Int] ~> as[MyClass] // even though this is an Any

    q(a, 12) shouldBe MyClass("foobar").validNec
    q(a, 4) shouldBe MyClass("foobar").validNec
  }

  it("should be ok to pass through an any-context decoder") {
    final case class MyClass(name: String)

    implicit val myClassDecoder: ContextualDecoder[JString, MyClass, Int] = { (in, context) =>
      if (in.value.length < context)
        MyClass(in.value).validNec
      else
        InvalidValue(in, "input string was too long according to context").invalidNec
    }
    val a: JAny = JString("foobar", JLocation(1, 1), JPointer.Root)

    val q = root[JAny, Int] ~> as[JString] ~> as[MyClass]
    val r = q(a, 8)

    q(a, 8) shouldBe MyClass("foobar").validNec
  }
}
