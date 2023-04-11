//// bateman -- Copyright 2021 -- Justin Patterson
////
//// Licensed under the Apache License, Version 2.0 (the "License");
//// you may not use this file except in compliance with the License.
//// You may obtain a copy of the License at
////
//// http://www.apache.org/licenses/LICENSE-2.0
////
//// Unless required by applicable law or agreed to in writing, software
//// distributed under the License is distributed on an "AS IS" BASIS,
//// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
//// See the License for the specific language governing permissions and
//// limitations under the License.
//
//package org.scalawag.bateman.jsonapi.generic
//
//import org.scalatest.funspec.AnyFunSpec
//import org.scalatest.matchers.should.Matchers
//import org.scalawag.bateman.json.ParserTestUtils
//import org.scalawag.bateman.jsonapi.{ResourceIdentifierCodec, ResourceObjectCodec}
//import org.scalawag.bateman.jsonapi.decoding.{
//  ResourceIdentifier,
//  ResourceIdentifierDecoder,
//  ResourceObject,
//  ResourceObjectDecoder
//}
//import shapeless.tag.@@
//
//class ScaleTest extends AnyFunSpec with Matchers with ParserTestUtils {
//
//  it("should handle twenty coproducts (codec)") {
//    sealed trait MyTrait
//    final case class MyClass01(@Id id: String) extends MyTrait
//    final case class MyClass02(@Id id: String) extends MyTrait
//    final case class MyClass03(@Id id: String) extends MyTrait
//    final case class MyClass04(@Id id: String) extends MyTrait
//    final case class MyClass05(@Id id: String) extends MyTrait
//    final case class MyClass06(@Id id: String) extends MyTrait
//    final case class MyClass07(@Id id: String) extends MyTrait
//    final case class MyClass08(@Id id: String) extends MyTrait
//    final case class MyClass09(@Id id: String) extends MyTrait
//    final case class MyClass10(@Id id: String) extends MyTrait
//    final case class MyClass11(@Id id: String) extends MyTrait
//    final case class MyClass12(@Id id: String) extends MyTrait
//    final case class MyClass13(@Id id: String) extends MyTrait
//    final case class MyClass14(@Id id: String) extends MyTrait
//    final case class MyClass15(@Id id: String) extends MyTrait
//    final case class MyClass16(@Id id: String) extends MyTrait
//    final case class MyClass17(@Id id: String) extends MyTrait
//    final case class MyClass18(@Id id: String) extends MyTrait
//    final case class MyClass19(@Id id: String) extends MyTrait
//    final case class MyClass20(@Id id: String) extends MyTrait
//
//    object MyClass01 {
//      implicit val decoder = semiauto.deriveResourceIdentifierDecoderForCaseClass[MyClass01]("my_class_01")
//    }
//    object MyClass02 {
//      implicit val decoder = semiauto.deriveResourceIdentifierDecoderForCaseClass[MyClass02]("my_class_02")
//    }
//    object MyClass03 {
//      implicit val decoder = semiauto.deriveResourceIdentifierDecoderForCaseClass[MyClass03]("my_class_03")
//    }
//    object MyClass04 {
//      implicit val decoder = semiauto.deriveResourceIdentifierDecoderForCaseClass[MyClass04]("my_class_04")
//    }
//    object MyClass05 {
//      implicit val decoder = semiauto.deriveResourceIdentifierDecoderForCaseClass[MyClass05]("my_class_05")
//    }
//    object MyClass06 {
//      implicit val decoder = semiauto.deriveResourceIdentifierDecoderForCaseClass[MyClass06]("my_class_06")
//    }
//    object MyClass07 {
//      implicit val decoder = semiauto.deriveResourceIdentifierDecoderForCaseClass[MyClass07]("my_class_07")
//    }
//    object MyClass08 {
//      implicit val decoder = semiauto.deriveResourceIdentifierDecoderForCaseClass[MyClass08]("my_class_08")
//    }
//    object MyClass09 {
//      implicit val decoder = semiauto.deriveResourceIdentifierDecoderForCaseClass[MyClass09]("my_class_09")
//    }
//    object MyClass10 {
//      implicit val decoder = semiauto.deriveResourceIdentifierDecoderForCaseClass[MyClass10]("my_class_10")
//    }
//    object MyClass11 {
//      implicit val decoder = semiauto.deriveResourceIdentifierDecoderForCaseClass[MyClass11]("my_class_11")
//    }
//    object MyClass12 {
//      implicit val decoder = semiauto.deriveResourceIdentifierDecoderForCaseClass[MyClass12]("my_class_12")
//    }
//    object MyClass13 {
//      implicit val decoder = semiauto.deriveResourceIdentifierDecoderForCaseClass[MyClass13]("my_class_13")
//    }
//    object MyClass14 {
//      implicit val decoder = semiauto.deriveResourceIdentifierDecoderForCaseClass[MyClass14]("my_class_14")
//    }
//    object MyClass15 {
//      implicit val decoder = semiauto.deriveResourceIdentifierDecoderForCaseClass[MyClass15]("my_class_15")
//    }
//    object MyClass16 {
//      implicit val decoder = semiauto.deriveResourceIdentifierDecoderForCaseClass[MyClass16]("my_class_16")
//    }
//    object MyClass17 {
//      implicit val decoder = semiauto.deriveResourceIdentifierDecoderForCaseClass[MyClass17]("my_class_17")
//    }
//    object MyClass18 {
//      implicit val decoder = semiauto.deriveResourceIdentifierDecoderForCaseClass[MyClass18]("my_class_18")
//    }
//    object MyClass19 {
//      implicit val decoder = semiauto.deriveResourceIdentifierDecoderForCaseClass[MyClass19]("my_class_19")
//    }
//    object MyClass20 {
//      implicit val decoder = semiauto.deriveResourceIdentifierDecoderForCaseClass[MyClass20]("my_class_20")
//    }
//    object MyTrait {
//      implicit val decoder = semiauto.deriveResourceIdentifierDecoderForTrait[MyTrait]()
//    }
//
//    ResourceIdentifierDecoder[MyTrait]
//      .decode(parseAs[ResourceIdentifier]("""{"type":"my_class_20","id":"ID"}"""), null)
//      .shouldSucceed shouldBe MyClass20("ID")
//  }
//
////  it("should handle twenty coproducts (codec)") {
////    sealed trait MyTrait
////    final case class MyClass01(@Id id: String) extends MyTrait
////    final case class MyClass02(@Id id: String) extends MyTrait
////    final case class MyClass03(@Id id: String) extends MyTrait
////    final case class MyClass04(@Id id: String) extends MyTrait
////    final case class MyClass05(@Id id: String) extends MyTrait
////    final case class MyClass06(@Id id: String) extends MyTrait
////    final case class MyClass07(@Id id: String) extends MyTrait
////    final case class MyClass08(@Id id: String) extends MyTrait
////    final case class MyClass09(@Id id: String) extends MyTrait
////    final case class MyClass10(@Id id: String) extends MyTrait
////    final case class MyClass11(@Id id: String) extends MyTrait
////    final case class MyClass12(@Id id: String) extends MyTrait
////    final case class MyClass13(@Id id: String) extends MyTrait
////    final case class MyClass14(@Id id: String) extends MyTrait
////    final case class MyClass15(@Id id: String) extends MyTrait
////    final case class MyClass16(@Id id: String) extends MyTrait
////    final case class MyClass17(@Id id: String) extends MyTrait
////    final case class MyClass18(@Id id: String) extends MyTrait
////    final case class MyClass19(@Id id: String) extends MyTrait
////    final case class MyClass20(@Id id: String) extends MyTrait
////
////    object MyClass01 {
////      implicit val codec = semiauto.deriveResourceIdentifierCodecForCaseClass[MyClass01]("my_class_01")
////      implicit val decoder = codec.decoder
////    }
////    object MyClass02 {
////      implicit val codec = semiauto.deriveResourceIdentifierCodecForCaseClass[MyClass02]("my_class_02")
////      implicit val decoder = codec.decoder
////    }
////    object MyClass03 {
////      implicit val codec = semiauto.deriveResourceIdentifierCodecForCaseClass[MyClass03]("my_class_03")
////      implicit val decoder = codec.decoder
////    }
////    object MyClass04 {
////      implicit val codec = semiauto.deriveResourceIdentifierCodecForCaseClass[MyClass04]("my_class_04")
////      implicit val decoder = codec.decoder
////    }
////    object MyClass05 {
////      implicit val codec = semiauto.deriveResourceIdentifierCodecForCaseClass[MyClass05]("my_class_05")
////      implicit val decoder = codec.decoder
////    }
////    object MyClass06 {
////      implicit val codec = semiauto.deriveResourceIdentifierCodecForCaseClass[MyClass06]("my_class_06")
////      implicit val decoder = codec.decoder
////    }
////    object MyClass07 {
////      implicit val codec = semiauto.deriveResourceIdentifierCodecForCaseClass[MyClass07]("my_class_07")
////      implicit val decoder = codec.decoder
////    }
////    object MyClass08 {
////      implicit val codec = semiauto.deriveResourceIdentifierCodecForCaseClass[MyClass08]("my_class_08")
////      implicit val decoder = codec.decoder
////    }
////    object MyClass09 {
////      implicit val codec = semiauto.deriveResourceIdentifierCodecForCaseClass[MyClass09]("my_class_09")
////      implicit val decoder = codec.decoder
////    }
////    object MyClass10 {
////      implicit val codec = semiauto.deriveResourceIdentifierCodecForCaseClass[MyClass10]("my_class_10")
////      implicit val decoder = codec.decoder
////    }
////    object MyClass11 {
////      implicit val codec = semiauto.deriveResourceIdentifierCodecForCaseClass[MyClass11]("my_class_11")
////      implicit val decoder = codec.decoder
////    }
////    object MyClass12 {
////      implicit val codec = semiauto.deriveResourceIdentifierCodecForCaseClass[MyClass12]("my_class_12")
////      implicit val decoder = codec.decoder
////    }
////    object MyClass13 {
////      implicit val codec = semiauto.deriveResourceIdentifierCodecForCaseClass[MyClass13]("my_class_13")
////      implicit val decoder = codec.decoder
////    }
////    object MyClass14 {
////      implicit val codec = semiauto.deriveResourceIdentifierCodecForCaseClass[MyClass14]("my_class_14")
////      implicit val decoder = codec.decoder
////    }
////    object MyClass15 {
////      implicit val codec = semiauto.deriveResourceIdentifierCodecForCaseClass[MyClass15]("my_class_15")
////      implicit val decoder = codec.decoder
////    }
////    object MyClass16 {
////      implicit val codec = semiauto.deriveResourceIdentifierCodecForCaseClass[MyClass16]("my_class_16")
////      implicit val decoder = codec.decoder
////    }
////    object MyClass17 {
////      implicit val codec = semiauto.deriveResourceIdentifierCodecForCaseClass[MyClass17]("my_class_17")
////      implicit val decoder = codec.decoder
////    }
////    object MyClass18 {
////      implicit val codec = semiauto.deriveResourceIdentifierCodecForCaseClass[MyClass18]("my_class_18")
////      implicit val decoder = codec.decoder
////    }
////    object MyClass19 {
////      implicit val codec = semiauto.deriveResourceIdentifierCodecForCaseClass[MyClass19]("my_class_19")
////      implicit val decoder = codec.decoder
////    }
////    object MyClass20 {
////      implicit val codec = semiauto.deriveResourceIdentifierCodecForCaseClass[MyClass20]("my_class_20")
////      implicit val decoder = codec.decoder
////    }
////    object MyTrait {
////      implicit val codec: ResourceIdentifierCodec[MyTrait] = semiauto.deriveResourceIdentifierCodecForTrait[MyTrait]
////      implicit val decoder = codec.decoder
////    }
////
////    ResourceIdentifierCodec[MyTrait]
////      .decode(parseAs[ResourceIdentifier]("""{"type":"my_class_20","id":"ID"}"""), null)
////      .shouldSucceed shouldBe MyClass20("ID")
////  }
////
//  it("should handle twenty fields (decoder)") {
//    final case class MyClass(
//        @Id id: String,
//        @Attribute f01: Int,
//        @Attribute f02: Int,
//        @Attribute f03: Int,
//        @Attribute f04: Int,
//        @Attribute f05: Int,
//        @Attribute f06: Int,
//        @Attribute f07: Int,
//        @Attribute f08: Int,
//        @Attribute f09: Int,
//        @Attribute f10: Int,
//        @Attribute f11: Int,
//        @Attribute f12: Int,
//        @Attribute f13: Int,
//        @Attribute f14: Int,
//        @Attribute f15: Int,
//        @Attribute f16: Int,
//        @Attribute f17: Int,
//        @Attribute f18: Int,
//        @Attribute f19: Int,
//        @Attribute f20: Int,
//    )
//
//    object MyClass {
//      implicit val decoder = semiauto.deriveResourceObjectDecoderForCaseClass[MyClass]("my_class")
//    }
//
//    val ro = parseAs[ResourceObject]("""
//      {
//        "type": "my_class",
//        "id": "ID",
//        "attributes": {
//          "f01": 1,
//          "f02": 2,
//          "f03": 3,
//          "f04": 4,
//          "f05": 5,
//          "f06": 6,
//          "f07": 7,
//          "f08": 8,
//          "f09": 9,
//          "f10": 10,
//          "f11": 11,
//          "f12": 12,
//          "f13": 13,
//          "f14": 14,
//          "f15": 15,
//          "f16": 16,
//          "f17": 17,
//          "f18": 18,
//          "f19": 19,
//          "f20": 20
//        }
//      }
//    """)
//
//    ResourceObjectDecoder[MyClass]
//      .decode(ro, null)
//      .shouldSucceed shouldBe MyClass("ID", 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20)
//  }
//
//  it("should handle twenty fields (codec)") {
//    final case class MyClass(
//        @Id id: String,
//        @Attribute f01: Int,
//        @Attribute f02: Int,
//        @Attribute f03: Int,
//        @Attribute f04: Int,
//        @Attribute f05: Int,
//        @Attribute f06: Int,
//        @Attribute f07: Int,
//        @Attribute f08: Int,
//        @Attribute f09: Int,
//        @Attribute f10: Int,
//        @Attribute f11: Int,
//        @Attribute f12: Int,
//        @Attribute f13: Int,
//        @Attribute f14: Int,
//        @Attribute f15: Int,
//        @Attribute f16: Int,
//        @Attribute f17: Int,
//        @Attribute f18: Int,
//        @Attribute f19: Int,
//        @Attribute f20: Int,
//    )
//
//    object MyClass {
//      val codec = semiauto.deriveResourceObjectCodecForCaseClass[MyClass]("my_class")
//    }
//
//    val ro = parseAs[ResourceObject]("""
//      {
//        "type": "my_class",
//        "id": "ID",
//        "attributes": {
//          "f01": 1,
//          "f02": 2,
//          "f03": 3,
//          "f04": 4,
//          "f05": 5,
//          "f06": 6,
//          "f07": 7,
//          "f08": 8,
//          "f09": 9,
//          "f10": 10,
//          "f11": 11,
//          "f12": 12,
//          "f13": 13,
//          "f14": 14,
//          "f15": 15,
//          "f16": 16,
//          "f17": 17,
//          "f18": 18,
//          "f19": 19,
//          "f20": 20
//        }
//      }
//    """)
//
//    ResourceObjectDecoder[MyClass](MyClass.codec)
//      .decode(ro, null)
//      .shouldSucceed shouldBe MyClass("ID", 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20)
////    ResourceObjectDecoder[MyClass](MyClass.codec).decode(ro, null)
//  }
//
//  it("should handle twenty optional attributes (codec)") {
//    final case class MyClass(
//        @Id id: String,
//        @Attribute f01: Option[Int] = None,
//        @Attribute f02: Option[Int] = None,
//        @Attribute f03: Option[Int] = None,
//        @Attribute f04: Option[Int] = None,
//        @Attribute f05: Option[Int] = None,
//        @Attribute f06: Option[Int] = None,
//        @Attribute f07: Option[Int] = None,
//        @Attribute f08: Option[Int] = None,
//        @Attribute f09: Option[Int] = None,
//        @Attribute f10: Option[Int] = None,
//        @Attribute f11: Option[Int] = None,
//        @Attribute f12: Option[Int] = None,
//        @Attribute f13: Option[Int] = None,
//        @Attribute f14: Option[Int] = None,
//        @Attribute f15: Option[Int] = None,
//        @Attribute f16: Option[Int] = None,
//        @Attribute f17: Option[Int] = None,
//        @Attribute f18: Option[Int] = None,
//        @Attribute f19: Option[Int] = None,
//        @Attribute f20: Option[Int] = None,
//    )
//
//    implicit val codec = semiauto.unchecked.deriveResourceObjectCodecForCaseClass[MyClass]("my_class")
//
//    val ro = parseAs[ResourceObject]("""
//      {
//        "type": "my_class",
//        "id": "ID"
//      }
//    """)
//
//    ResourceObjectDecoder[MyClass].decode(ro, null).shouldSucceed shouldBe MyClass("ID")
//  }
//
//  it("should handle twenty optional metas (codec)") {
//    final case class MyClass(
//        @Id id: String,
//        @Meta f01: Option[Int] = None,
//        @Meta f02: Option[Int] = None,
//        @Meta f03: Option[Int] = None,
//        @Meta f04: Option[Int] = None,
//        @Meta f05: Option[Int] = None,
//        @Meta f06: Option[Int] = None,
//        @Meta f07: Option[Int] = None,
//        @Meta f08: Option[Int] = None,
//        @Meta f09: Option[Int] = None,
//        @Meta f10: Option[Int] = None,
//        @Meta f11: Option[Int] = None,
//        @Meta f12: Option[Int] = None,
//        @Meta f13: Option[Int] = None,
//        @Meta f14: Option[Int] = None,
//        @Meta f15: Option[Int] = None,
//        @Meta f16: Option[Int] = None,
//        @Meta f17: Option[Int] = None,
//        @Meta f18: Option[Int] = None,
//        @Meta f19: Option[Int] = None,
//        @Meta f20: Option[Int] = None,
//    )
//
//    implicit val codec = semiauto.unchecked.deriveResourceObjectCodecForCaseClass[MyClass]("my_class")
//
//    val ro = parseAs[ResourceObject]("""
//      {
//        "type": "my_class",
//        "id": "ID"
//      }
//    """)
//
//    ResourceObjectDecoder[MyClass].decode(ro, null).shouldSucceed shouldBe MyClass("ID")
//  }
//
//  it("should handle twenty optional relationships (codec)") {
//    final case class MyReferent(@Id id: String)
//
//    final case class MyClass(
//        @Id id: String,
//        @Relationship f01: Option[MyReferent] = None,
//        @Relationship f02: Option[MyReferent] = None,
//        @Relationship f03: Option[MyReferent] = None,
//        @Relationship f04: Option[MyReferent] = None,
//        @Relationship f05: Option[MyReferent] = None,
//        @Relationship f06: Option[MyReferent] = None,
//        @Relationship f07: Option[MyReferent] = None,
//        @Relationship f08: Option[MyReferent] = None,
//        @Relationship f09: Option[MyReferent] = None,
//        @Relationship f10: Option[MyReferent] = None,
//        @Relationship f11: Option[MyReferent] = None,
//        @Relationship f12: Option[MyReferent] = None,
//        @Relationship f13: Option[MyReferent] = None,
//        @Relationship f14: Option[MyReferent] = None,
//        @Relationship f15: Option[MyReferent] = None,
//        @Relationship f16: Option[MyReferent] = None,
//        @Relationship f17: Option[MyReferent] = None,
//        @Relationship f18: Option[MyReferent] = None,
//        @Relationship f19: Option[MyReferent] = None,
//        @Relationship f20: Option[MyReferent] = None,
//    )
//
//    implicit val rcodec = semiauto.deriveResourceIdentifierCodecForCaseClass[MyReferent]("my_referent")
//    implicit val codec = semiauto.deriveResourceObjectCodecForCaseClass[MyClass]("my_class")
//
//    val ro = parseAs[ResourceObject]("""
//      {
//        "type": "my_class",
//        "id": "ID"
//      }
//    """)
//
//    ResourceObjectDecoder[MyClass].decode(ro, null).shouldSucceed shouldBe MyClass("ID")
//  }
//
//}
