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

package org.scalawag.bateman.json.enumeratum

import cats.syntax.either._
import enumeratum.EnumEntry.Lowercase
import enumeratum.{Enum, EnumEntry}
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import org.scalawag.bateman.json.focus.JFocus
import org.scalawag.bateman.json.{InvalidValue, JAny, JNumber, JString, JsonTypeMismatch}
import org.scalawag.bateman.json.syntax._
import org.scalawag.bateman.json.focus.weak._

class BatemanEnumTest extends AnyFunSpec with Matchers {
  sealed trait Color extends EnumEntry with Lowercase

  case object Color extends Enum[Color] with BatemanEnum[Color] {
    case object Red extends Color
    case object Green extends Color
    case object Blue extends Color

    val values = findValues
  }

  it("should encode") {
    (Color.Red: Color).toJAny shouldBe JString("red")
  }

  it("should decode") {
    JString("blue").asRootFocus.decode[Color] shouldBe Color.Blue.rightNec
  }

  it("should fail to decode (value)") {
    val in = JString("Red").asRootFocus
    in.decode[Color] shouldBe InvalidValue(in, "'Red' is not a member of enum Color").leftNec
  }

  it("should fail to decode (type)") {
    val in: JFocus[JAny] = JNumber(8).asRootFocus
    in.decode[Color] shouldBe JsonTypeMismatch(in, JString).leftNec
  }
}
