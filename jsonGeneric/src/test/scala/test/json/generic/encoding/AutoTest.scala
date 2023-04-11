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

import cats.syntax.functor._
import org.scalawag.bateman.json.literal.JsonStringContext
import org.scalawag.bateman.json.syntax._
import org.scalawag.bateman.json.{Decoder, JAny, JAnyEncoder, JNumber, JNumberDecoder, JNumberEncoder}
import test.json.BatemanTestBase

object AutoTest {
  object EncoderOnly {
    class Value(private val a: Int)
    object Value {
      def apply(a: Int): Value = new Value(a)
      implicit val encoder: JAnyEncoder[Value] = v => JNumber(v.a)
    }

    sealed trait MyTrait
    final case class MyClass(value: Value) extends MyTrait
  }

  object DecoderOnly {
    class Value(private val a: Int) {
      def publica: Int = a
    }
    object Value {
      def apply(a: Int): Value = new Value(a)
      implicit val decoder: JNumberDecoder[Value] = Decoder.jnumberToIntDecoder.map(Value(_))
    }

    sealed trait MyTrait
    final case class MyClass(value: Value) extends MyTrait
  }
}

class AutoTest extends BatemanTestBase {

  it("should generate a case class encoder even if a decoder can't be generated") {
    import AutoTest.EncoderOnly._
    import org.scalawag.bateman.json.generic.auto._

    MyClass(Value(17)) shouldEncodeTo json"""{"value":17}"""
  }

  it("should generate a trait encoder even if a decoder can't be generated") {
    import AutoTest.EncoderOnly._
    import org.scalawag.bateman.json.generic.auto._

    (MyClass(Value(17)): MyTrait) shouldEncodeTo json"""{"type":"MyClass","value":17}"""
  }

  it("should generate a case class decoder even if a encoder can't be generated") {
    import AutoTest.DecoderOnly._
    import org.scalawag.bateman.json.generic.auto._

    inside(json"""{"value":17}""".asRootFocus.decode[MyClass].shouldSucceed) {
      case MyClass(v) => v.publica shouldBe 17
    }
  }

  it("should generate a trait decoder even if a encoder can't be generated") {
    import AutoTest.DecoderOnly._
    import org.scalawag.bateman.json.generic.auto._

    inside(json"""{"type":"MyClass","value":17}""".asRootFocus.decode[MyTrait].shouldSucceed) {
      case MyClass(v) => v.publica shouldBe 17
    }
  }
}
