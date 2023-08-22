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

package org.scalawag.bateman.json.decoding

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import org.scalawag.bateman.json.DataDrivenTestUtils

class JAnyTest extends AnyFunSpec with Matchers with DataDrivenTestUtils {
  describe("JAny") {}

  describe("JString") {
    val cases = Iterable[DataDrivenTestCase[(String, Either[(Int, String), (Int, String)])]](
    )

    it("should convert a string to a string") {}
  }
}