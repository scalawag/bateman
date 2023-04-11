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

import enumeratum.values._
import cats.syntax.either._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import org.scalawag.bateman.json.{InvalidValue, JAny, JLocation, JNumber, JPointer, JString, JsonTypeMismatch}
import org.scalawag.bateman.json.syntax._

class BatemanValueEnumTest extends AnyFunSpec with Matchers {
  describe("Int") {
    sealed abstract class ShirtSize(val value: Int) extends IntEnumEntry

    case object ShirtSize extends IntEnum[ShirtSize] with IntBatemanEnum[ShirtSize] {
      case object Small extends ShirtSize(1)
      case object Medium extends ShirtSize(2)
      case object Large extends ShirtSize(3)

      val values = findValues
    }

    it("should encode") {
      (ShirtSize.Small: ShirtSize).toJAny shouldBe JNumber(1)
    }

    it("should decode") {
      JNumber(3).asRootFocus.decode[ShirtSize] shouldBe ShirtSize.Large.rightNec
    }

    it("should fail to decode (value)") {
      val in = JNumber(10).asRootFocus
      in.decode[ShirtSize] shouldBe InvalidValue(in, "10 is not a member of enum ShirtSize").leftNec
    }
  }

  describe("Char") {
    sealed abstract class TriStateBool(val value: Char) extends CharEnumEntry

    case object TriStateBool extends CharEnum[TriStateBool] with CharBatemanEnum[TriStateBool] {

      case object True extends TriStateBool('T')
      case object False extends TriStateBool('F')
      case object Maybe extends TriStateBool('M')

      val values = findValues
    }

    it("should encode") {
      (TriStateBool.False: TriStateBool).toJAny shouldBe JString("F")
    }

    it("should decode") {
      JString("M").asRootFocus.decode[TriStateBool] shouldBe TriStateBool.Maybe.rightNec
    }

    it("should fail to decode (value)") {
      val in = JString("?").asRootFocus
      in.decode[TriStateBool] shouldBe InvalidValue(in, "? is not a member of enum TriStateBool").leftNec
    }

    it("should fail to decode (length)") {
      val in = JString("Maybe").asRootFocus
      in.decode[TriStateBool] shouldBe InvalidValue(in, "'Maybe' is not a string of exactly length one").leftNec
    }
  }
}
