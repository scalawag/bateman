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

import org.scalawag.bateman.jsonapi.generic.Annotations.{Attribute, IncludedRelationship}
import test.json.BatemanTestBase

import java.util.concurrent.atomic.AtomicInteger
import org.scalawag.bateman.jsonapi.generic.auto._
import org.scalawag.bateman.jsonapi.syntax._
import LocalIdEncoderTest._
import org.scalawag.bateman.jsonapi.generic.encoding.LidGenerator

import java.util.UUID

object LocalIdEncoderTest {
  case class MyReferent(@Attribute a: Int)
  case class MyReferrer(@IncludedRelationship rel: MyReferent)
}

class LocalIdEncoderTest extends BatemanTestBase {
  it("should generate a local ID using default generator") {
    val out = MyReferrer(MyReferent(77)).toDocument
    println(out.spaces2)
    import org.scalawag.bateman.jsonapi.lens._
    out.asRootFocus(data ~> relationship("rel") ~> data ~> lid).decode[UUID].shouldSucceed
  }

  it("should generate a local ID using a custom generator") {
    implicit val lidder: LidGenerator = new LidGenerator {
      private val next = new AtomicInteger(0)
      override def apply(): String = next.incrementAndGet().toString
    }

    val out = MyReferrer(MyReferent(77)).toDocument
    println(out.spaces2)
    import org.scalawag.bateman.jsonapi.lens._
    out.asRootFocus(data ~> relationship("rel") ~> data ~> lid).decode[String].shouldSucceed.toInt shouldBe 1
  }
}
