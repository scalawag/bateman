package org.scalawag.bateman.json.decoding.parser

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import org.scalawag.bateman.json.decoding.parser

class OverflowTest extends AnyFunSpec with Matchers {
  private val factor = 10000

  it("should parse long documents") {
    val in = Stream("""["ABC",""".toStream, Stream.fill(factor)(""""ABC",""".toStream).flatten, "]".toStream).flatten
    parser.toEvents(in).size // force evaluation of the stream without blowing out memory
  }

  it("should parse deep documents") {
    val in = Stream.fill(factor)("""{"a":""".toStream).flatten #::: Stream.fill(factor)('}')
    parser.toEvents(in).size // force evaluation of the stream without blowing out memory
  }
}
