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
import org.scalawag.bateman.json.generic.naming.{CamelCase, CaseTransformation, PascalCase, SnakeCase}
import org.scalawag.bateman.json.generic.semiauto._

import java.util.concurrent.TimeUnit

object StateDecoderBenchmark {
  sealed trait LineItem
  final case class Product(
      itemId: String,
      price: String,
      currency: String,
      addedAt: String,
      reference: String,
      warehouseId: String
  ) extends LineItem
  final case class Service(
      itemId: String,
      price: String,
      currency: String,
      addedAt: String,
      reference: String,
      warehouseId: String
  ) extends LineItem
  final case class OrderState(orderId: String, status: String, items: List[LineItem])

  /** Unknown fields rejected, snake_case field and class names. */
  object Strict {
    implicit val config: Config = Config.default.copy(
      allowUnknownFields = false,
      fieldNameMapping = CaseTransformation(CamelCase, SnakeCase),
      classNameMapping = CaseTransformation(PascalCase, SnakeCase),
    )
    implicit val productDecoder: JObjectDecoder[Product] = deriveDecoderForCaseClass[Product]()
    implicit val serviceDecoder: JObjectDecoder[Service] = deriveDecoderForCaseClass[Service]()
    implicit val lineItemDecoder: JObjectDecoder[LineItem] = deriveDecoderForTrait[LineItem]()
    implicit val stateDecoder: JObjectDecoder[OrderState] = deriveDecoderForCaseClass[OrderState]()
  }
}

/** Decodes a state holding `size` items of a sealed trait, the way a deserializer does: parse the text, then decode
  * the root with a derived decoder.
  */
@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 2, time = 1)
@Measurement(iterations = 3, time = 1)
@Fork(1)
class StateDecoderBenchmark {
  import StateDecoderBenchmark._

  @Param(Array("100", "1000", "4000"))
  var size: Int = _

  var text: String = _
  var root: JFocus[JAny] = _

  @Setup
  def setup(): Unit = {
    val items = (1 to size)
      .map { n =>
        val kind = if (n % 2 == 0) "product" else "service"
        s"""{"type":"$kind","item_id":"item-$n","price":"12.34","currency":"USD","added_at":"2026-11-01T12:00:00Z",""" +
          s""""reference":"ref-$n","warehouse_id":"w-1"}"""
      }
      .mkString("[", ",", "]")
    text = s"""{"order_id":"o-1","status":"shipped","items":$items}"""
    root = parseUnsafe(text)
  }

  /** Decodes an already parsed document. */
  @Benchmark
  def decode(): Any = root.narrow[JObject].flatMap(_.decode(Strict.stateDecoder))

  /** Parses and decodes, as a deserializer does for each document it reads. */
  @Benchmark
  def parseAndDecode(): Any = parseUnsafe(text).narrow[JObject].flatMap(_.decode(Strict.stateDecoder))
}
