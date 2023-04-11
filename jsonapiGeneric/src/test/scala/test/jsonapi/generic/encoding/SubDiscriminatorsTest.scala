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

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import org.scalawag.bateman.json.lens._
import org.scalawag.bateman.json.generic.Cardinality
import org.scalawag.bateman.json.syntax._
import org.scalawag.bateman.json.{JAny, JObject, JString, NotNull, Null, Nullable}
import org.scalawag.bateman.jsonapi.encoding.IncludeSpec
import org.scalawag.bateman.jsonapi.generic.Annotations._
import org.scalawag.bateman.jsonapi.generic.semiauto.unchecked._
import org.scalawag.bateman.jsonapi.syntax._
import org.scalawag.bateman.jsonapi.encoding.{FieldsSpec, ResourceEncoder}
import org.scalawag.bateman.jsonapi.lens

import java.time.Instant
import scala.reflect.runtime.universe.typeOf
import SubDiscriminatorsTest._
import org.scalawag.bateman.json.generic.Discriminators._

object SubDiscriminatorsTest {

//  sealed trait Status
//  case object Started extends Status
//  case object Aborted extends Status
//  case object Canceled extends Status

  sealed trait State {
    val id: String
//    val status: Status
  }

  case class Started(
      @Id id: String,
      //      @Meta status: Status,
      @Attribute startedAt: Instant
  ) extends State

  sealed trait Terminated extends State {
    val terminatedAt: Instant
  }

  case class Completed(
      @Id id: String,
      //      @Meta status: Status,
      @Attribute terminatedAt: Instant,
      @Attribute result: Float
  ) extends Terminated

  case class Failed(
      @Id id: String,
      //      @Meta status: Status,
      @Attribute terminatedAt: Instant,
      @Attribute failurecode: Int,
      @Attribute failureReason: String,
  ) extends Terminated

  implicit val startedEncoder: ResourceEncoder[Started] =
    deriveResourceEncoderForCaseClass[Started]("cash_flow")
  implicit val completedEncoder: ResourceEncoder[Completed] =
    deriveResourceEncoderForCaseClass[Completed]("cash_flow")
  implicit val failedEncoder: ResourceEncoder[Failed] =
    deriveResourceEncoderForCaseClass[Failed]("cash_flow")

  implicit val terminatedEncoder: ResourceEncoder[Terminated] =
    deriveResourceEncoderForTrait[Terminated](
      lens.meta("status"),
      CustomDiscriminator(
//        forType[Started]("started"),
        forType[Completed]("completed"),
        forType[Failed]("failed"),
      )
    )

  implicit val stateEncoder: ResourceEncoder[State] =
    deriveResourceEncoderForTrait[State](
      lens.meta("status"),
      CustomDiscriminator(
        forType[Started]("started".toJAny),
        forType[Completed]("completed".toJAny),
        forType[Failed]("failed".toJAny),
      )
    )

}

class SubDiscriminatorsTest extends AnyFunSpec with Matchers /*with ParserTestUtils with DataDrivenTestUtils*/ {
  it("should encode properly") {
    val s1: State = Started("A", Instant.now())
    val s2: State = Completed("A", Instant.now(), 4.5f)
    val s3: State = Failed("A", Instant.now(), 34, "Things went south.")
    println(s1.toDocument.spaces2)
    println(s2.toDocument.spaces2)
    println(s3.toDocument.spaces2)
  }
}
