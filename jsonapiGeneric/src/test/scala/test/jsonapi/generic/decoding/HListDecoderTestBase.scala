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

package test.jsonapi.generic.decoding

import cats.data.NonEmptyChain
import org.scalactic.source.Position
import org.scalawag.bateman.json.focus.{Single, JFocus}
import org.scalawag.bateman.json.{JAny, JAnyDecoder, JError, JErrors}
import org.scalawag.bateman.json.lens._
import org.scalawag.bateman.json.focus.weak._
import test.json.BatemanTestBase

trait HListDecoderTestBase extends BatemanTestBase {

  case class Input[A <: JAny](
      text: JAny,
      deepNav: JLens[Single, JAny, A],
      resourcePath: JLens[Single, JAny, JAny] = focus
  )(implicit
      val name: sourcecode.Name
  ) {
    val json: JFocus[JAny] = text.asRootFocus
    val deepFocus: JFocus[A] = json(deepNav).shouldSucceed

    def succeedsWith[B](expected: B)(implicit position: Position, dec: JAnyDecoder[B]): Unit =
      it(s"should succeed on ${name.value}") {
        json(resourcePath).shouldSucceed.decode[B].shouldSucceed shouldBe expected
      }

    def failsWith[B](fn: JFocus[A] => JError)(implicit position: Position, dec: JAnyDecoder[B]): Unit =
      it(s"should fail on ${name.value}") {
        json(resourcePath).shouldSucceed.decode[B].shouldFailSingle.fullDescription shouldBe fn(
          deepFocus
        ).fullDescription
      }

    def failsWithMultiple[B](
        fn: JFocus[A] => NonEmptyChain[JError]
    )(implicit position: Position, dec: JAnyDecoder[B]): Unit =
      it(s"should fail on ${name.value}") {
        JErrors.formatErrorReport(json(resourcePath).shouldSucceed.decode[B].shouldFail) shouldBe JErrors
          .formatErrorReport(fn(deepFocus))
      }
  }

}
