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

package test.json

import cats.data.NonEmptyChain
import org.scalacheck.Gen
import org.scalactic.source.Position
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.{Assertion, Inside}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import org.scalawag.bateman.json
import org.scalawag.bateman.json._
import org.scalawag.bateman.json.JErrors.formatErrorReport
import org.scalawag.bateman.json.JType.Summoner
import org.scalawag.bateman.json.focus.JRootFocus
import org.scalawag.bateman.json.syntax._
import org.typelevel.discipline.scalatest.FunSpecDiscipline

import java.time.{Instant, LocalDate, LocalDateTime, LocalTime, ZoneOffset}
import java.util.UUID
import scala.reflect.ClassTag

abstract class BatemanTestBase
    extends AnyFunSpec
    with Matchers
    with Inside
    with ScalaCheckPropertyChecks
    with FunSpecDiscipline
    with JAnyGenerators {
  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(sizeRange = 100, workers = 4)

  def truncate(in: String, len: Int = 80): String =
    if (in.length > 80)
      s"'${in.take(80)}'..."
    else
      s"'$in'"

  implicit class JFieldTestOps(me: JField) {
    def hasLocations: Boolean = me.name.hasLocations || me.value.hasLocations
  }

  implicit class JAnyTestOps(me: JAny) {
    def shouldHaveNoLocations: Assertion =
      if (me.hasLocations)
        fail("JAny should be free of location info, but it's not")
      else
        succeed

    def hasLocations: Boolean =
      me match {
        case arr: JArray  => arr.location.nonEmpty || arr.items.exists(_.location.nonEmpty)
        case obj: JObject => obj.location.nonEmpty || obj.fieldList.exists(_.hasLocations)
        case _            => me.location.nonEmpty
      }
  }

  def parse(in: String)(implicit pos: Position): JRootFocus[JAny] =
    json.parse(in).fold(e => fail(s"failure parsing test JSON: ${e.getMessage}"), identity)

  def parseAs[A <: JAny: ClassTag: Summoner](in: String)(implicit dec: JAnyDecoder[A], pos: Position): JRootFocus[A] = {
    val root = parse(in)
    root.narrow[A].fold(ee => fail(formatErrorReport(ee)), identity)
  }

  implicit class JResultOps[A](r: JResult[A]) {
    def shouldSucceed(implicit pos: Position): A =
      r.fold(
        ee => fail("operation should have succeeded but failed with:\n" + formatErrorReport(ee)),
        identity
      )

    def shouldFail(implicit pos: Position): NonEmptyChain[JError] =
      r.fold(
        identity,
        a => fail(s"operation should have failed but succeeded with value: $a"),
      )

    def shouldFailSingle(implicit pos: Position): JError =
      r.fold(
        ee =>
          ee.length match {
            case 1 => ee.head
            case n =>
              fail("operation should have failed with one error but failed with several:\n" + formatErrorReport(ee))
          },
        a => fail(s"operation should have failed but succeeded with value: $a"),
      )
  }

  implicit class JAnyOps(me: JAny) {
    def shouldRenderTo(you: JAny)(implicit pos: Position): Assertion = me.render shouldEqual you.render
  }

  implicit class RichEncodable[A: JAnyEncoder](a: A) {
    def shouldEncodeTo(json: JAny)(implicit position: Position): Unit =
      a.toJAny.render shouldBe json.render
  }

  val genLocalDate: Gen[LocalDate] =
    for {
      epochDay <- Gen.choose(0, 365 * 70)
    } yield LocalDate.ofEpochDay(epochDay)

  val genLocalTime: Gen[LocalTime] =
    for {
      h <- Gen.choose(0, 23)
      m <- Gen.choose(0, 59)
      s <- Gen.choose(0, 59)
    } yield LocalTime.of(h, m, s)

  val genLocalDateTime: Gen[LocalDateTime] =
    for {
      d <- genLocalDate
      t <- genLocalTime
    } yield t.atDate(d)

  val genInstant: Gen[Instant] =
    for {
      dt <- genLocalDateTime
      nanos <- Gen.oneOf( // choose arbitrarily 0, 3, 6 or 9 digits of fractional-second precision
        Gen.const(0),
        Gen.choose(0, 999).map(_ * 1000000),
        Gen.choose(0, 999999).map(_ * 1000),
        Gen.choose(0, 999999999).map(_ * 1),
      )
    } yield Instant.ofEpochSecond(dt.toEpochSecond(ZoneOffset.UTC)).plusNanos(nanos)

  val genUuid: Gen[UUID] =
    UUID.randomUUID()
}
