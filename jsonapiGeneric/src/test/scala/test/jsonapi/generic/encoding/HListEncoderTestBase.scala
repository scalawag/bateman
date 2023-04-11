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

package test.jsonapi.generic.encoding

import org.scalactic.source.Position
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import org.scalawag.bateman.json._
import org.scalawag.bateman.jsonapi.encoding.ResourceEncoder
import org.scalawag.bateman.jsonapi.encoding.EncodeResult
import org.scalawag.bateman.jsonapi.encoding.EncodeResult.formatErrorReport
import org.scalawag.bateman.jsonapi.syntax._
import test.json.BatemanTestBase

trait HListEncoderTestBase extends BatemanTestBase {
  implicit class RichEncodable[A: ResourceEncoder](a: A) {
    def shouldEncodeTo(json: JObject)(implicit position: Position): Unit =
      a.toDocument.render shouldBe json.render
  }

  implicit class EncodeResultOps[A](r: EncodeResult[A]) {
    def shouldSucceed(implicit pos: Position): A =
      r.fold(
        ee => fail("operation should have succeeded but failed with:\n" + formatErrorReport(ee)),
        identity
      )
  }
}
