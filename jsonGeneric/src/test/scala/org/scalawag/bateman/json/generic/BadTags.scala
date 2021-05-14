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

package org.scalawag.bateman.json.generic

import org.scalawag.bateman.json.syntax._
import org.scalawag.bateman.json.JObjectCodec
import org.scalawag.bateman.json.decoding.{JLocation, JObject, JObjectDecoder, JPointer}
import org.scalawag.bateman.json.encoding.JObjectEncoder
import org.scalawag.bateman.json.generic.codec.CaseClassCodec
import org.scalawag.bateman.json.generic.decoding.{CaseClassDecoder, JSource}
import org.scalawag.bateman.json.generic.encoding.CaseClassEncoder
import org.scalawag.bateman.json.generic.semiauto._
import shapeless.tag.@@

object BadTags {
  trait MyTag extends Tag
  case class X(a: Int /*@@ MyTag*/, src: JSource @@ SourceTag)
  implicit val codec: CaseClassCodec[X, Any] = deriveCodecForCaseClass[X, Any]("something")

  sealed trait C
  case class A(n: Int) extends C
  case class B(s: String) extends C

  implicit val cc = Config.default.copy(discriminatorField = "typ")
  implicit val a = deriveEncoderForCaseClass[A]()
  implicit val b = deriveEncoderForCaseClass[B]()
  implicit val c = deriveEncoderForTrait[C]("somethin")

  def main(args: Array[String]): Unit = {
    println(X(17, JSource(JObject(Nil, JLocation(1, 1), JPointer.Root))).toJAny.render)

//    println(A(31).toJAny.render)
//    println(A(31).toJAny(a).render)
  }
}
