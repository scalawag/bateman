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

package org.scalawag.bateman.json

import cats.data.NonEmptyChain
import org.scalactic.source.Position
import org.scalatest.funspec.AnyFunSpec
import org.scalawag.bateman.json.decoding.DecodeError.formatErrorReport
import org.scalawag.bateman.json.decoding.{DecodeError, DecodeResult, JAnyDecoder}
import org.scalawag.bateman.json.encoding.JAny

import scala.collection.compat.immutable.LazyList

trait ParserTestUtils { _: AnyFunSpec =>
  def parse(in: String)(implicit pos: Position): decoding.JAny =
    decoding.parser.toJAny(in.to(LazyList)).fold(e => fail(s"failure parsing test JSON: ${e.description}"), identity)

  def parseAs[A](in: String)(implicit dec: JAnyDecoder[A], pos: Position): A =
    dec.decode(parse(in), ()).fold(ee => fail(formatErrorReport(ee)), identity)

  implicit class DecodeResultOps[A](r: DecodeResult[A]) {
    def shouldSucceed(implicit pos: Position): A =
      r.fold(
        ee => fail("operation should have succeeded but failed with:\n" + formatErrorReport(ee)),
        identity
      )

    def shouldFail(implicit pos: Position): NonEmptyChain[DecodeError] =
      r.fold(
        identity,
        a => fail(s"operation should have failed but succeeded with value: $a"),
      )

    def shouldFailSingle(implicit pos: Position): DecodeError =
      r.fold(
        ee =>
          ee.length match {
            case 1 => ee.head
            case n =>
              fail("operation should have failed with one error but failed with several:\n" + formatErrorReport(ee))
          },
        a => fail(s"operation should have failed but succeeded with value: $a"),
      )
  }
}
