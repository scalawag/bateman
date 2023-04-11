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

package test.json.generic.encoding

import org.scalawag.bateman.json.{JObjectCodec, JObjectDecoder, JObjectEncoder, UnexpectedValue}
import org.scalawag.bateman.json.generic.Config
import org.scalawag.bateman.json.generic.naming.{CamelCase, PascalCase, SnakeCase}
import org.scalawag.bateman.json.lens._
import org.scalawag.bateman.json.literal._
import org.scalawag.bateman.json.syntax._
import test.json.BatemanTestBase
import test.json.generic.encoding.MultipleDiscriminatorsTest._

import java.time.Instant

object MultipleDiscriminatorsTest {

  sealed trait Event

  case class Started(
      id: String,
      startedAt: Instant
  ) extends Event

  sealed trait Terminated extends Event {
    val terminatedAt: Instant
  }

  case class Completed(
      id: String,
      terminatedAt: Instant,
      result: Float
  ) extends Terminated

  case class Failed(
      id: String,
      terminatedAt: Instant,
      failureCode: Int,
      failureReason: String,
  ) extends Terminated

}

class MultipleDiscriminatorsTest extends BatemanTestBase {

  describe("auto") {
    import org.scalawag.bateman.json.generic.auto._
    val now = Instant.now()

    it("should encode abstract Started properly") {
      val state: Event = Started("A", now)
      state shouldEncodeTo json"""{"type": "Started", "id": "A", "startedAt": $now}"""
    }

    it("should encode abstract Completed properly") {
      val state: Event = Completed("B", now, 4.5f)
      state shouldEncodeTo json"""{"type": "Completed", "id": "B", "terminatedAt": $now, "result": 4.5}"""
    }

    it("should encode abstract Failed properly") {
      val state: Event = Failed("C", now, 34, "Things went south.")
      state shouldEncodeTo json"""
        {
          "type": "Failed",
          "id": "C",
          "terminatedAt": $now,
          "failureCode": 34,
          "failureReason": "Things went south."
        }
      """
    }
  }

  describe("auto with config") {
    import org.scalawag.bateman.json.generic.auto._
    implicit val config: Config = Config(
      fieldNameMapping = CamelCase to SnakeCase,
      classNameMapping = PascalCase to SnakeCase
    )
    val now = Instant.now()

    it("should encode abstract Started properly") {
      val state: Event = Started("A", now)
      state shouldEncodeTo json"""{"type": "started", "id": "A", "started_at": $now}"""
    }

    it("should encode abstract Completed properly") {
      val state: Event = Completed("B", now, 4.5f)
      state shouldEncodeTo json"""{"type": "completed", "id": "B", "terminated_at": $now, "result": 4.5}"""
    }

    it("should encode abstract Failed properly") {
      val state: Event = Failed("C", now, 34, "Things went south.")
      state shouldEncodeTo json"""
        {
          "type": "failed",
          "id": "C",
          "terminated_at": $now,
          "failure_code": 34,
          "failure_reason": "Things went south."
        }
      """
    }
  }

  describe("custom multi-discriminator encoder") {
    import org.scalawag.bateman.json.generic.semiauto.unchecked._
    val now = Instant.now()

    implicit val config: Config = Config(classNameMapping = PascalCase to SnakeCase)

    implicit val startedEncoder: JObjectEncoder[Started] = deriveEncoderForCaseClass[Started]()
    implicit val completedEncoder: JObjectEncoder[Completed] = deriveEncoderForCaseClass[Completed]()
    implicit val failedEncoder: JObjectEncoder[Failed] = deriveEncoderForCaseClass[Failed]()

    import org.scalawag.bateman.json.generic.Discriminators._

    implicit val terminatedEncoder: JObjectEncoder[Terminated] =
      deriveEncoderForTrait[Terminated]("state")

    implicit val eventEncoder: JObjectEncoder[Event] =
      deriveEncoderForTrait[Event](
        "event",
        CustomDiscriminator(duplicateValuesForbidden = false)(
          forType[Started]("started"),
          forType[Terminated]("terminated"),
        )
      )

    it("should encode abstract Started properly") {
      val event: Event = Started("A", now)
      event shouldEncodeTo json"""
        {
          "event": "started",
          "id": "A",
          "startedAt": $now
        }
      """
    }

    it("should encode abstract Completed properly") {
      val event: Event = Completed("B", now, 4.5f)
      event shouldEncodeTo json"""
        {
          "event": "terminated",
          "state": "completed",
          "id": "B",
          "terminatedAt": $now,
          "result": 4.5
        }
      """
    }

    it("should encode abstract Failed properly") {
      val event: Event = Failed("C", now, 34, "Things went south.")
      event shouldEncodeTo json"""
        {
          "event": "terminated",
          "state": "failed",
          "id": "C",
          "terminatedAt": $now,
          "failureCode": 34,
          "failureReason": "Things went south."
        }
      """
    }

    it("should encode abstract Completed properly as Terminated") {
      val event: Terminated = Completed("B", now, 4.5f)
      event shouldEncodeTo json"""
        {
          "state": "completed",
          "id": "B",
          "terminatedAt": $now,
          "result": 4.5
        }
      """
    }

    it("should encode abstract Failed properly as Terminated") {
      val event: Terminated = Failed("C", now, 34, "Things went south.")
      event shouldEncodeTo json"""
        {
          "state": "failed",
          "id": "C",
          "terminatedAt": $now,
          "failureCode": 34,
          "failureReason": "Things went south."
        }
      """
    }
  }

  describe("custom multi-discriminator decoder") {
    import org.scalawag.bateman.json.generic.semiauto.unchecked._
    val now = Instant.now()

    implicit val config: Config = Config(
      fieldNameMapping = CamelCase to SnakeCase,
      classNameMapping = PascalCase to SnakeCase,
      allowUnknownFields = false
    )

    implicit val startedDecoder: JObjectDecoder[Started] = deriveDecoderForCaseClass[Started]()
    implicit val completedDecoder: JObjectDecoder[Completed] = deriveDecoderForCaseClass[Completed]()
    implicit val failedDecoder: JObjectDecoder[Failed] = deriveDecoderForCaseClass[Failed]()

    import org.scalawag.bateman.json.generic.Discriminators._

    implicit val terminatedDecoder: JObjectDecoder[Terminated] =
      deriveDecoderForTrait[Terminated]("state")

    implicit val eventDecoder: JObjectDecoder[Event] =
      deriveDecoderForTrait[Event](
        "event",
        CustomDiscriminator(duplicateValuesForbidden = false)(
          forType[Started]("started"),
          forType[Terminated]("terminated"),
        )
      )

    it("should decode abstract Started properly") {
      json"""
        {
          "event": "started",
          "id": "A",
          "started_at": $now
        }
      """.asRootFocus.decode[Event].shouldSucceed shouldBe Started("A", now)
    }

    it("should decode abstract Completed properly") {
      json"""
        {
          "event": "terminated",
          "state": "completed",
          "id": "B",
          "terminated_at": $now,
          "result": 4.5
        }
      """.asRootFocus.decode[Event].shouldSucceed shouldBe Completed("B", now, 4.5f)
    }

    it("should decode abstract Failed properly") {
      json"""
        {
          "event": "terminated",
          "state": "failed",
          "id": "C",
          "terminated_at": $now,
          "failure_code": 34,
          "failure_reason": "Things went south."
        }
      """.asRootFocus.decode[Event].shouldSucceed shouldBe Failed("C", now, 34, "Things went south.")
    }

    it("should decode abstract Completed properly as Terminated") {
      json"""
        {
          "state": "completed",
          "id": "B",
          "terminated_at": $now,
          "result": 4.5
        }
      """.asRootFocus.decode[Terminated].shouldSucceed shouldBe Completed("B", now, 4.5f)
    }

    it("should decode abstract Failed properly as Terminated") {
      json"""
        {
          "state": "failed",
          "id": "C",
          "terminated_at": $now,
          "failure_code": 34,
          "failure_reason": "Things went south."
        }
      """.asRootFocus.decode[Terminated].shouldSucceed shouldBe Failed("C", now, 34, "Things went south.")
    }

    it("should reject discriminator it knows nothing about") {
      val f = json"""
        {
          "event": "terminated",
          "state": "failed",
          "id": "C",
          "terminated_at": $now,
          "failure_code": 34,
          "failure_reason": "Things went south."
        }
      """.asRootFocus
      f.decode[Terminated].shouldFailSingle shouldBe UnexpectedValue(f.field("event").shouldSucceed)
    }

  }

  describe("custom multi-discriminator codec") {
    import org.scalawag.bateman.json.generic.semiauto.unchecked._
    val now = Instant.now()

    implicit val config: Config = Config(
      fieldNameMapping = CamelCase to SnakeCase,
      classNameMapping = PascalCase to SnakeCase
    )

    implicit val startedCodec: JObjectCodec[Started] = deriveCodecForCaseClass[Started]()
    implicit val completedCodec: JObjectCodec[Completed] = deriveCodecForCaseClass[Completed]()
    implicit val failedCodec: JObjectCodec[Failed] = deriveCodecForCaseClass[Failed]()

    import org.scalawag.bateman.json.generic.Discriminators._

    implicit val terminatedCodec: JObjectCodec[Terminated] =
      deriveCodecForTrait[Terminated]("state")

    implicit val eventCodec: JObjectCodec[Event] =
      deriveCodecForTrait[Event](
        "event",
        CustomDiscriminator(duplicateValuesForbidden = false)(
          forType[Started]("started"),
          forType[Terminated]("terminated"),
        )
      )

    it("should encode abstract Started properly") {
      val event: Event = Started("A", now)
      event shouldEncodeTo json"""
        {
          "event": "started",
          "id": "A",
          "started_at": $now
        }
      """
    }

    it("should encode abstract Completed properly") {
      val event: Event = Completed("B", now, 4.5f)
      event shouldEncodeTo json"""
        {
          "event": "terminated",
          "state": "completed",
          "id": "B",
          "terminated_at": $now,
          "result": 4.5
        }
      """
    }

    it("should encode abstract Failed properly") {
      val event: Event = Failed("C", now, 34, "Things went south.")
      event shouldEncodeTo json"""
        {
          "event": "terminated",
          "state": "failed",
          "id": "C",
          "terminated_at": $now,
          "failure_code": 34,
          "failure_reason": "Things went south."
        }
      """
    }

    it("should round-trip properly") {
      val s1: Event = Started("A", Instant.now())
      val s2: Event = Completed("B", Instant.now(), 4.5f)
      val s3: Event = Failed("C", Instant.now(), 34, "Things went south.")

      s1.toJAny.asRootFocus.decode[Event].shouldSucceed shouldBe s1
      s2.toJAny.asRootFocus.decode[Event].shouldSucceed shouldBe s2
      s3.toJAny.asRootFocus.decode[Event].shouldSucceed shouldBe s3

      s2.toJAny.asRootFocus.decode[Terminated].shouldSucceed shouldBe s2
      s3.toJAny.asRootFocus.decode[Terminated].shouldSucceed shouldBe s3

      s1.toJAny.asRootFocus.decode[Started].shouldSucceed shouldBe s1
      s2.toJAny.asRootFocus.decode[Completed].shouldSucceed shouldBe s2
      s3.toJAny.asRootFocus.decode[Failed].shouldSucceed shouldBe s3
    }
  }

}
