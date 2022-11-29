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

package org.scalawag.bateman.json.enumeratum

import enumeratum.values._
import cats.syntax.validated._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import org.scalawag.bateman.json.decoding.{InvalidValue, JLocation, JPointer, JsonTypeMismatch}
import org.scalawag.bateman.json.{decoding, encoding}
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
      (ShirtSize.Small: ShirtSize).toJAny shouldBe encoding.JNumber(1)
    }

    it("should decode") {
      decoding.JNumber("3", JLocation(1, 1), JPointer.Root).as[ShirtSize] shouldBe ShirtSize.Large.validNec
    }

    it("should fail to decode (value)") {
      val in = decoding.JNumber("10", JLocation(1, 1), JPointer.Root)
      in.as[ShirtSize] shouldBe InvalidValue(in, "10 is not a member of enum ShirtSize").invalidNec
    }

    it("should fail to decode (type)") {
      val in: decoding.JAny = decoding.JString("L", JLocation(1, 1), JPointer.Root)
      in.as[ShirtSize] shouldBe JsonTypeMismatch(in, decoding.JNumber).invalidNec
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
      (TriStateBool.False: TriStateBool).toJAny shouldBe encoding.JString("F")
    }

    it("should decode") {
      decoding.JString("M", JLocation(1, 1), JPointer.Root).as[TriStateBool] shouldBe TriStateBool.Maybe.validNec
    }

    it("should fail to decode (value)") {
      val in = decoding.JString("?", JLocation(1, 1), JPointer.Root)
      in.as[TriStateBool] shouldBe InvalidValue(in, "? is not a member of enum TriStateBool").invalidNec
    }

    it("should fail to decode (length)") {
      val in = decoding.JString("Maybe", JLocation(1, 1), JPointer.Root)
      in.as[TriStateBool] shouldBe InvalidValue(in, "expecting a one-character-long string").invalidNec
    }

    it("should fail to decode (type)") {
      val in: decoding.JAny = decoding.JNumber("8", JLocation(1, 1), JPointer.Root)
      in.as[TriStateBool] shouldBe JsonTypeMismatch(in, decoding.JString).invalidNec
    }
  }
}
