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

package test.json.generic.decoding

import org.scalawag.bateman.json.{JObject, JObjectDecoder, UnexpectedValue}
import org.scalawag.bateman.json.generic.Config
import org.scalawag.bateman.json.generic.semiauto._
import test.json.BatemanTestBase

object LongListDecoderTest {
  // Five fields, so the decoder's set of consumed fields is a hashed Set rather than one of Set1 to Set4.
  final case class Item(kind: String, id: String, amount: String, currency: String, addedAt: String)
  final case class Order(id: String, items: List[Item])

  implicit val config: Config = Config(allowUnknownFields = false)
  implicit val itemDecoder: JObjectDecoder[Item] = deriveDecoderForCaseClass[Item]()
  implicit val orderDecoder: JObjectDecoder[Order] = deriveDecoderForCaseClass[Order]()
}

class LongListDecoderTest extends BatemanTestBase {
  import LongListDecoderTest._

  private def order(size: Int) =
    parseAs[JObject](
      (1 to size)
        .map(n => s"""{"kind":"product","id":"item-$n","amount":"1.00","currency":"USD","addedAt":"2026-11-01"}""")
        .mkString("""{"id":"o-1","items":[""", ",", "]}")
    )

  private def seconds[A](block: => A): (A, Double) = {
    val start = System.nanoTime()
    val result = block
    (result, (System.nanoTime() - start) / 1e9)
  }

  it("should decode 2000 items strictly without hashing the document per field") {
    // The unknown-field check used to keep the consumed field foci in a Set, and hashing a focus hashes the whole
    // document, so this took about 14 seconds on a laptop JVM. It now takes a few tens of milliseconds; the limit
    // only has to catch the quadratic version.
    val doc = order(2000)
    val (decoded, elapsed) = seconds(orderDecoder.decode(doc))
    decoded.shouldSucceed.items should have size 2000
    elapsed should be < 2.0
  }

  it("should still reject an unknown field") {
    val tx = parseAs[JObject](
      """{"kind":"product","id":"item-1","amount":"1.00","currency":"USD","addedAt":"2026-11-01","memo":"x"}"""
    )
    val errors = itemDecoder.decode(tx).shouldFail
    errors.iterator.toList.collect { case UnexpectedValue(f) => f.pointer.toString } shouldBe List("/memo")
  }
}
