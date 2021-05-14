// bateman -- Copyright 2021 -- Justin Patterson
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

package org.scalawag.bateman.json.decoding

import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Gen._
import org.scalacheck.Arbitrary._

trait JAnyGenerators {

  val pointer: Gen[JPointer] = {
    val unescaped = oneOf(
      choose(0x0000.toChar, 0x002e.toChar),
      choose(0x0030.toChar, 0x007d.toChar),
      choose(0x007f.toChar, 0xffff.toChar)
    ).map(_.toString)

    val escaped = oneOf("~0", "~1")

    val char = frequency(1 -> escaped, 100 -> unescaped)

    val refToken = listOf(char).map(_.mkString)

    listOf(refToken).map(_.foldLeft(JPointer.Root: JPointer)(_ / _))
  }

  val location: Gen[JLocation] =
    for {
      line <- arbitrary[Int]
      column <- arbitrary[Int]
    } yield JLocation(line, column)

  val jstring: Gen[JString] = {
    val controlChar = oneOf('\n', '\b', '\t', '\r', '\f')

    val nonControlChar = choose(0x0020.toChar, 0xffff.toChar)

    val validJsonChar = frequency(30 -> nonControlChar, 1 -> controlChar)

    val length = frequency(
      10 -> choose(0, 0),
      100 -> choose(1, 32),
      10 -> choose(32, 2048),
      1 -> choose(2049, 1048576)
    )

    for {
      len <- length
      chars <- listOfN(len, validJsonChar).map(_.mkString)
      location <- location
      pointer <- pointer
    } yield JString(chars, location, pointer)
  }

  val jnull: Gen[JNull] =
    for {
      location <- location
      pointer <- pointer
    } yield JNull(location, pointer)

  val jnumber: Gen[JNumber] = {
    // TODO: This needs to generate more diverse numbers (exps, negs, fractionals, etc.)
    for {
      len <- choose(1, 10)
      chars <- listOfN(len, choose('0', '9')).map(_.mkString)
      location <- location
      pointer <- pointer
    } yield JNumber(chars, location, pointer)
  }

  val jboolean: Gen[JBoolean] =
    for {
      b <- arbitrary[Boolean]
      location <- location
      pointer <- pointer
    } yield JBoolean(b, location, pointer)

  val jobject: Gen[JObject] = {
    val field =
      for {
        n <- jstring
        v <- jany
      } yield JField(n, v)

    for {
      fieldCount <- choose(0, 8)
      fields <- listOfN(fieldCount, field).retryUntil(ff => ff.map(_.name.value).distinct.size == ff.size)
      location <- location
      pointer <- pointer
    } yield JObject(fields, location, pointer) // TODO: it's possible to get a random name collision here
  }

  val jarray: Gen[JArray] =
    for {
      len <- choose(0, 8)
      items <- listOfN(len, jany)
      location <- location
      pointer <- pointer
    } yield JArray(items, location, pointer)

  def jany: Gen[JAny] =
    frequency(
      20 -> jstring,
      10 -> jnumber,
      4 -> jboolean,
      4 -> jnull,
      2 -> jobject,
      1 -> jarray,
    )

  implicit val arbJAny: Arbitrary[JAny] = Arbitrary(jany)
  implicit val arbJString: Arbitrary[JString] = Arbitrary(jstring)
  implicit val arbJNumber: Arbitrary[JNumber] = Arbitrary(jnumber)
  implicit val arbJBoolean: Arbitrary[JBoolean] = Arbitrary(jboolean)
  implicit val arbJNull: Arbitrary[JNull] = Arbitrary(jnull)
  implicit val arbJObject: Arbitrary[JObject] = Arbitrary(jobject)
  implicit val arbJArray: Arbitrary[JArray] = Arbitrary(jarray)
}
