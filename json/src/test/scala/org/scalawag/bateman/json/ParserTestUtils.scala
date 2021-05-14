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

import org.scalactic.source.Position
import org.scalatest.funspec.AnyFunSpec
import org.scalawag.bateman.json.decoding.DecodeError.formatErrorReport
import org.scalawag.bateman.json.decoding.{DecodeResult, JAnyDecoder}

trait ParserTestUtils { _: AnyFunSpec =>
  def parse(in: String)(implicit pos: Position): decoding.JAny =
    decoding.parser.toJAny(in.toStream).fold(e => fail(s"failure parsing test JSON: ${e.description}"), identity)

  def parseAs[A](in: String)(implicit dec: JAnyDecoder[A], pos: Position): A =
    dec.decode(parse(in), ()).fold(ee => fail(formatErrorReport(ee)), identity)

  implicit class DecodeResultOps[A](r: DecodeResult[A]) {
    def shouldSucceed(implicit pos: Position): A =
      r.fold(
        ee => fail("operation should have succeeded but failed with:\n " + formatErrorReport(ee)),
        identity
      )
  }
}
