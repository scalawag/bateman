package org.scalawag.bateman.json.parser2

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import org.scalawag.bateman.json.decoding.parser
import org.scalawag.bateman.json.decoding.parser.tokenizer.CharStream
import org.scalawag.bateman.json.parser2

import java.util.zip.GZIPInputStream
import scala.io.Source
import scala.util.Try

class Performance extends AnyFunSpec with Matchers {
  def bigJson = {
    val in = new GZIPInputStream(Thread.currentThread().getContextClassLoader.getResourceAsStream("large-file.json.gz"))
    try {
      Source.fromInputStream(in).mkString
    } finally {
      in.close()
    }
  }
  val bigString = s"""{"a": "${"A" * 1024 * 1024}"}"""
  val smallJson = """
    {
      "a": 12.3,
      "b": false,
      "c": 5,
      "d": [ "g", "h", 4, true, null],
      "e": {
        "e": "f",
        "f": null,
        "g": true
      }
    }
  """

  def time[A](fn: => A): (Long, A) = {
    val start = System.currentTimeMillis
    Try(fn).map((System.currentTimeMillis - start, _)).get
  }

  it("should parse small documents") {
    runComparison(smallJson, 10000)
  }

  // fastparse takes several minutes to parse this large JSON, so this test is disabled by default.
  ignore("should parse large documents") {
    runComparison(bigJson, 1)
  }

  it("should parse large strings") {
    runComparison(bigString, 8)
  }

  it("should tokenize big JSON") {
    val (t, tokens) = time {
      org.scalawag.bateman.json.decoding.parser.tokenizer.Tokenizer
        .tokenize(CharStream(bigJson.toStream))
    }

    val (e, events) = time {
      org.scalawag.bateman.json.decoding.parser.eventizer.Eventizer.eventize(tokens)
    }

    val (d, documents) = time {
      org.scalawag.bateman.json.decoding.parser.documenter.Documentizer.documentize(events)
    }

    println(s"tokenize: $t ms")
    println(s"eventize: $e ms")
    println(s"documentize: $d ms")
  }

  private def runComparison(input: String, iterations: Int): Unit = {
    val (ft, fr) = time {
      for (_ <- 1 to iterations) yield parser2.JsonParser.parse(input)
    }
    println(s"$ft x")

    val src = input.toStream
    val (bt, br) = time {
      for (_ <- 1 to iterations) yield parser.toJAny(src)
    }
    println(s"$bt y")

    println(s"fastparse:$ft builtin:$bt")

    // Make sure all parsed documents are the same.
    fr.tail.forall(_ == fr.head) shouldBe true
    fr.length shouldBe iterations
    br.tail.forall(_ == br.head) shouldBe true
    br.length shouldBe iterations
    fr.head shouldBe br.head
  }
}
