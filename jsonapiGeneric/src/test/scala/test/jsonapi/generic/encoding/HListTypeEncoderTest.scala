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

import org.scalawag.bateman.jsonapi.generic.Annotations.Type
import test.json.BatemanTestBase
import HListTypeEncoderTest._
import org.scalawag.bateman.json.literal._
import org.scalawag.bateman.json.syntax._

object HListTypeEncoderTest {
  case class MyClass(@Type b: String)
}

class HListTypeEncoderTest extends BatemanTestBase {
  it("should ignore @Type field") {
    import org.scalawag.bateman.jsonapi.generic.auto._
    MyClass("arbitrary_type_string").toJAny shouldEncodeTo json"""{"type": "MyClass"}"""
  }
}
