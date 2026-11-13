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

package org.scalawag.bateman.json.benchmarks

import org.openjdk.jmh.annotations._
import org.scalawag.bateman.json._
import org.scalawag.bateman.json.focus.JFocus
import org.scalawag.bateman.json.generic.Config
import org.scalawag.bateman.json.generic.semiauto._

import java.util.concurrent.TimeUnit

object DerivedDecoderBenchmark {
  final case class Item(kind: String, id: String, amount: String, currency: String, addedAt: String)
  final case class Order(id: String, items: List[Item])

  object Lenient {
    implicit val itemDecoder: JObjectDecoder[Item] = deriveDecoderForCaseClass[Item]()
    implicit val orderDecoder: JObjectDecoder[Order] = deriveDecoderForCaseClass[Order]()
  }

  object Strict {
    implicit val config: Config = Config(allowUnknownFields = false)
    implicit val itemDecoder: JObjectDecoder[Item] = deriveDecoderForCaseClass[Item]()
    implicit val orderDecoder: JObjectDecoder[Order] = deriveDecoderForCaseClass[Order]()
  }
}

/** Decodes an order with `size` items through derived case class decoders.
  *
  * With `allowUnknownFields = false`, the Scala 2 derived decoder collects the field foci it consumed into a Set and
  * looks every field up in it. Hashing a focus hashes its parent chain up to the root, the whole document.
  */
@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 2, time = 1)
@Measurement(iterations = 3, time = 1)
@Fork(1)
class DerivedDecoderBenchmark {
  import DerivedDecoderBenchmark._

  @Param(Array("100", "1000", "4000"))
  var size: Int = _

  var valid: JFocus[JObject] = _
  var invalid: JFocus[JObject] = _

  private def order(id: Int => String): JFocus[JObject] = {
    val txs = (1 to size)
      .map(n => s"""{"kind":"product","id":${id(n)},"amount":"12.34","currency":"USD","addedAt":"2026-11-01"}""")
      .mkString("[", ",", "]")
    parseUnsafe(s"""{"id":"o-1","items":$txs}""")
      .narrow[JObject]
      .fold(e => throw new IllegalStateException(e.toString), identity)
  }

  @Setup
  def setup(): Unit = {
    valid = order(n => s""""item-$n"""")
    invalid = order(n => n.toString) // every id is a number, so every item fails
  }

  @Benchmark
  def decodeLenient(): Any = Lenient.orderDecoder.decode(valid)

  @Benchmark
  def decodeStrict(): Any = Strict.orderDecoder.decode(valid)

  /** Formats the errors from an order whose every item fails, as JErrors does for its message. */
  @Benchmark
  def formatErrors(): Any =
    Lenient.orderDecoder.decode(invalid).fold(JErrors.formatErrorReport, _ => throw new IllegalStateException)
}
