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
import org.scalawag.bateman.json.lens._

import java.util.concurrent.TimeUnit

/** Cursors over a long list, shaped like a JSON:API to-many relationship (`{"type": ..., "id": ...}` per item).
  *
  * Every list cursor checks that its foci share one root. When that check hashed the root, building a cursor over n
  * items cost n times the document, so these grew quadratically with `size`.
  */
@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 3, time = 2)
@Measurement(iterations = 5, time = 2)
@Fork(1)
class JCursorBenchmark {
  @Param(Array("100", "1000", "4000"))
  var size: Int = _

  var root: JFocus[JObject] = _

  @Setup
  def setup(): Unit = {
    val items = (1 to size).map(n => s"""{"type":"item","id":"item-$n"}""").mkString("[", ",", "]")
    val text = s"""{"data":{"type":"order","id":"o-1","relationships":{"items":{"data":$items}}}}"""
    root = parseUnsafe(text).narrow[JObject].fold(e => throw new IllegalStateException(e.toString), identity)
  }

  private def items = "data" ~> "relationships" ~> "items" ~> "data" ~> *

  /** Builds the cursor over the list items. */
  @Benchmark
  def listCursor(): Any = root(items)

  /** Builds a cursor over each item's id and decodes them all. */
  @Benchmark
  def decodeIds(): Any = root(items ~> "id" ~> narrowTo[JString]).flatMap(_.decode[String])

  /** Replaces every item of the list. */
  @Benchmark
  def modifyItems(): Any = root(items).map(_.modify(_ => JNull))

  /** Deletes every item of the list. */
  @Benchmark
  def deleteItems(): Any = root(items).flatMap(_.delete())
}
