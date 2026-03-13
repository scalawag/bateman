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

package test.jsonapi.generic.encoding

import org.scalawag.bateman.json.syntax._
import org.scalawag.bateman.jsonapi.generic.Annotations._
import org.scalawag.bateman.jsonapi.generic.semiauto._
import org.scalawag.bateman.jsonapi.syntax._
import org.scalawag.bateman.jsonapi.encoding.ResourceEncoder
import org.scalawag.bateman.json.lens._
import org.scalawag.bateman.jsonapi.lens._
import org.scalawag.bateman.json.lens.CreatableJLensOps
import test.json.BatemanTestBase
import org.scalawag.bateman.json.generic.Discriminators._

import java.time.Instant
import SubDiscriminatorsTest._
import org.scalawag.bateman.json.JString

object SubDiscriminatorsTest {

  sealed trait State {
    val id: String
  }

  case class Started(
      @Id id: String,
      @Attribute startedAt: Instant
  ) extends State

  sealed trait Terminated extends State {
    val terminatedAt: Instant
  }

  case class Completed(
      @Id id: String,
      @Attribute terminatedAt: Instant,
      @Attribute result: Float
  ) extends Terminated

  case class Failed(
      @Id id: String,
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
      meta("status"),
      CustomDiscriminator(
        forType[Completed]("completed"),
        forType[Failed]("failed"),
      )
    )

  implicit val stateEncoder: ResourceEncoder[State] =
    deriveResourceEncoderForTrait[State](
      meta("status"),
      CustomDiscriminator(
        forType[Started]("started".toJAny),
        forType[Terminated]("terminated".toJAny),
      )
    )
}

class SubDiscriminatorsTest extends BatemanTestBase {
  private val dataType = data ~> "type" ~> narrowTo[JString]
  private val dataMetaStatus = data ~> meta("status") ~> narrowTo[JString]

  it("should encode Started") {
    val doc = (Started("A", Instant.parse("2025-01-01T00:00:00Z")): State).toDocument.toJObject
    doc.asRootFocus(dataType).shouldSucceed.value.value shouldBe "cash_flow"
    doc.asRootFocus(dataMetaStatus).shouldSucceed.value.value shouldBe "started"
  }

  it("should encode Completed") {
    // The inner discriminator ("completed") overwrites the outer ("terminated") since both use the same lens path.
    val doc = (Completed("B", Instant.parse("2025-01-01T00:00:00Z"), 4.5f): State).toDocument.toJObject
    doc.asRootFocus(dataType).shouldSucceed.value.value shouldBe "cash_flow"
    doc.asRootFocus(dataMetaStatus).shouldSucceed.value.value shouldBe "completed"
  }

  it("should encode Failed") {
    val doc = (Failed("C", Instant.parse("2025-01-01T00:00:00Z"), 34, "Things went south."): State).toDocument.toJObject
    doc.asRootFocus(dataType).shouldSucceed.value.value shouldBe "cash_flow"
    doc.asRootFocus(dataMetaStatus).shouldSucceed.value.value shouldBe "failed"
  }
}
