package org.scalawag.bateman.json.decoding.parser

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import org.scalawag.bateman.json.decoding.parser
import scala.collection.compat.immutable.LazyList

class OverflowTest extends AnyFunSpec with Matchers {
  private val factor = 10000

  it("should parse long documents") {
    val in = LazyList(
      """["ABC",""".to(LazyList),
      LazyList.fill(factor)(""""ABC",""".to(LazyList)).flatten,
      "]".to(LazyList)
    ).flatten
    parser.toEvents(in).size // force evaluation of the stream without blowing out memory
  }

  it("should parse deep documents") {
    val in = LazyList.fill(factor)("""{"a":""".to(LazyList)).flatten #::: LazyList.fill(factor)('}')
    parser.toEvents(in).size // force evaluation of the stream without blowing out memory
  }
}
