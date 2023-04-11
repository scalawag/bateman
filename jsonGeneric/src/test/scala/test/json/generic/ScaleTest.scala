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

package test.json.generic

import cats.syntax.either._
import org.scalawag.bateman.json.JAnyDecoder
import org.scalawag.bateman.json.generic.semiauto.unchecked.deriveDecoderForCaseClass
import test.json.BatemanTestBase
import test.json.generic.ScaleTest.{MyBigClass, MyBigOptionClass}

class ScaleTest extends BatemanTestBase {
  it("should handle at least forty fields") {

    val json = parse("""
      {
        "f01": 1,
        "f02": 2,
        "f03": 3,
        "f04": 4,
        "f05": 5,
        "f06": 6,
        "f07": 7,
        "f08": 8,
        "f09": 9,
        "f10": 10,
        "f11": 11,
        "f12": 12,
        "f13": 13,
        "f14": 14,
        "f15": 15,
        "f16": 16,
        "f17": 17,
        "f18": 18,
        "f19": 19,
        "f20": 20,
        "f21": 1,
        "f22": 2,
        "f23": 3,
        "f24": 4,
        "f25": 5,
        "f26": 6,
        "f27": 7,
        "f28": 8,
        "f29": 9,
        "f30": 10,
        "f31": 11,
        "f32": 12,
        "f33": 13,
        "f34": 14,
        "f35": 15,
        "f36": 16,
        "f37": 17,
        "f38": 18,
        "f39": 19,
        "f40": 20
      }
    """)

    implicit val dec = deriveDecoderForCaseClass[MyBigClass]()
    JAnyDecoder[MyBigClass].decode(json) shouldBe MyBigClass(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17,
      18, 19, 20, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20).rightNec
  }

  it("should handle at least forty optional fields") {

    val json = parse("""
      {
        "f37": 17
      }
    """)

    implicit val dec = deriveDecoderForCaseClass[MyBigOptionClass]()
    JAnyDecoder[MyBigOptionClass].decode(json) shouldBe MyBigOptionClass(f37 = Some(17)).rightNec
  }

  it("should handle at least twenty coproducts") {
    import ScaleTest._
    import org.scalawag.bateman.json.generic.auto._

//    implicit val dec = deriveDecoderForTrait[MyBigTrait]()
    JAnyDecoder[MyBigTrait].decode(parse("""{"type":"MyClass20","id":"ID"}""")) shouldBe MyClass20("ID").rightNec
  }

}

object ScaleTest {
  sealed trait MyBigTrait
  final case class MyClass01(id: String) extends MyBigTrait
  final case class MyClass02(id: String) extends MyBigTrait
  final case class MyClass03(id: String) extends MyBigTrait
  final case class MyClass04(id: String) extends MyBigTrait
  final case class MyClass05(id: String) extends MyBigTrait
  final case class MyClass06(id: String) extends MyBigTrait
  final case class MyClass07(id: String) extends MyBigTrait
  final case class MyClass08(id: String) extends MyBigTrait
  final case class MyClass09(id: String) extends MyBigTrait
  final case class MyClass10(id: String) extends MyBigTrait
  final case class MyClass11(id: String) extends MyBigTrait
  final case class MyClass12(id: String) extends MyBigTrait
  final case class MyClass13(id: String) extends MyBigTrait
  final case class MyClass14(id: String) extends MyBigTrait
  final case class MyClass15(id: String) extends MyBigTrait
  final case class MyClass16(id: String) extends MyBigTrait
  final case class MyClass17(id: String) extends MyBigTrait
  final case class MyClass18(id: String) extends MyBigTrait
  final case class MyClass19(id: String) extends MyBigTrait
  final case class MyClass20(id: String) extends MyBigTrait

  final case class MyBigClass(
      f01: Int,
      f02: Int,
      f03: Int,
      f04: Int,
      f05: Int,
      f06: Int,
      f07: Int,
      f08: Int,
      f09: Int,
      f10: Int,
      f11: Int,
      f12: Int,
      f13: Int,
      f14: Int,
      f15: Int,
      f16: Int,
      f17: Int,
      f18: Int,
      f19: Int,
      f20: Int,
      f21: Int,
      f22: Int,
      f23: Int,
      f24: Int,
      f25: Int,
      f26: Int,
      f27: Int,
      f28: Int,
      f29: Int,
      f30: Int,
      f31: Int,
      f32: Int,
      f33: Int,
      f34: Int,
      f35: Int,
      f36: Int,
      f37: Int,
      f38: Int,
      f39: Int,
      f40: Int,
  )

  final case class MyBigOptionClass(
      f01: Option[Int] = None,
      f02: Option[Int] = None,
      f03: Option[Int] = None,
      f04: Option[Int] = None,
      f05: Option[Int] = None,
      f06: Option[Int] = None,
      f07: Option[Int] = None,
      f08: Option[Int] = None,
      f09: Option[Int] = None,
      f10: Option[Int] = None,
      f11: Option[Int] = None,
      f12: Option[Int] = None,
      f13: Option[Int] = None,
      f14: Option[Int] = None,
      f15: Option[Int] = None,
      f16: Option[Int] = None,
      f17: Option[Int] = None,
      f18: Option[Int] = None,
      f19: Option[Int] = None,
      f20: Option[Int] = None,
      f21: Option[Int] = None,
      f22: Option[Int] = None,
      f23: Option[Int] = None,
      f24: Option[Int] = None,
      f25: Option[Int] = None,
      f26: Option[Int] = None,
      f27: Option[Int] = None,
      f28: Option[Int] = None,
      f29: Option[Int] = None,
      f30: Option[Int] = None,
      f31: Option[Int] = None,
      f32: Option[Int] = None,
      f33: Option[Int] = None,
      f34: Option[Int] = None,
      f35: Option[Int] = None,
      f36: Option[Int] = None,
      f37: Option[Int] = None,
      f38: Option[Int] = None,
      f39: Option[Int] = None,
      f40: Option[Int] = None,
  )
}
