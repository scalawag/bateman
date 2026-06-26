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

package test.json.parser

import test.json.BatemanTestBase
import org.scalawag.bateman.json.parser

import scala.collection.compat.immutable.LazyList

class OverflowTest extends BatemanTestBase {
  private val factor = 10000

  it("should parse long documents") {
    val in = LazyList(
      """["ABC",""".to(LazyList),
      LazyList.fill(factor)(""""ABC",""".to(LazyList)).flatten,
      "]".to(LazyList)
    ).flatten
    parser.toEvents(in).size // force evaluation of the stream without blowing out memory
  }

  it("should parse deep documents") {
    val in = LazyList.fill(factor)("""{"a":""".to(LazyList)).flatten #::: LazyList.fill(factor)('}')
    parser.toEvents(in).size // force evaluation of the stream without blowing out memory
  }

  it("should parse a generated wide document with escapes") {
    val in = LazyList(
      """["""".to(LazyList),
      LazyList
        .fill(factor)("\\\"".to(LazyList))
        .flatten,
      "\"]".to(LazyList)
    ).flatten
    parser.toEvents(in).size // force evaluation of the stream without blowing out memory
  }
}
