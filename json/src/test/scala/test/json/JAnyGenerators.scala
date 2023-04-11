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

import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Gen._
import org.scalacheck.Arbitrary._
import org.scalawag.bateman.json._
import org.scalawag.bateman.json.JNull.JNullImpl
import org.scalawag.bateman.json.JPointer.{Index, Key}
import org.scalawag.bateman.json.focus.{JFieldFocus, JFocus, JRootFocus, JStrongFocus}
import org.scalawag.bateman.json.focus.weak._

import scala.util.Random

trait JAnyGenerators {

  val genJPointer: Gen[JPointer] = {
    val unescaped = oneOf(
      choose(0x0000.toChar, 0x002e.toChar),
      choose(0x0030.toChar, 0x007d.toChar),
      choose(0x007f.toChar, 0xffff.toChar)
    ).map(_.toString)

    val escaped = oneOf("~0", "~1")

    val char = frequency(1 -> escaped, 100 -> unescaped)

    val refToken = listOf(char).map(_.mkString)

    listOf(refToken).map(_.foldLeft(JPointer.Root: JPointer) {
      case (p, s) => p.field(s)
    })
  }

  val genJLocation: Gen[JLocation] =
    for {
      line <- choose(1, 500)
      column <- choose(1, 120)
    } yield JLocation(line, column)

  val genUnreasonableJString: Gen[JString] = {
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
      location <- genJLocation
    } yield JString(chars, Some(location))
  }

  val genJString: Gen[JString] = {
    for {
      len <- choose(4, 8)
      chars <- listOfN(len, choose('a', 'z')).map(_.mkString)
      location <- genJLocation
    } yield JString(chars, Some(location))
  }

  val genJNull: Gen[JNull] =
    for {
      location <- genJLocation
    } yield JNullImpl(Some(location))

  private val genFractional: Gen[String] =
    for {
      len <- choose(1, 12)
      digits <- listOfN(len, choose('0', '9')).map(_.mkString)
    } yield s".$digits"

  private val genExp: Gen[String] =
    for {
      e <- oneOf("E", "e")
      digits <- choose(-999999999, 999999999)
    } yield s"$e$digits"

  def genJNumberOf[A: Numeric: Arbitrary]: Gen[JNumber] =
    for {
      digits <- arbitrary[A]
      location <- genJLocation
    } yield JNumber(digits.toString, Some(location))

  val genJNumber: Gen[JNumber] = {
    for {
      nonfrac <- genJNumberOf[Long]
      frac <- oneOf(const(""), genFractional)
      exp <- oneOf(const(""), genExp)
    } yield JNumber(s"${nonfrac.value}$frac$exp", nonfrac.location)
  }

  val genJBoolean: Gen[JBoolean] =
    for {
      b <- arbitrary[Boolean]
      location <- genJLocation
    } yield JBoolean(b, Some(location))

  def genJField(keys: Gen[JString]): Gen[JField] =
    for {
      n <- keys
      v <- genJAny
    } yield JField(n, v)

  def genJObject(keys: Gen[JString] = genJString): Gen[JObject] =
    for {
      fieldCount <- choose(0, 8)
      fields <- listOfN(fieldCount, genJField(keys)).retryUntil(ff => ff.map(_.name.value).distinct.size == ff.size)
      location <- genJLocation
    } yield JObject(fields, Some(location)) // TODO: it's possible to get a random name collision here

  val genJObject: Gen[JObject] = genJObject()

  val genJObjectWithDuplicateFields: Gen[JObject] =
    for {
      fieldCount <- choose(2, 8)
      dupCount <- choose(2, fieldCount)
      dupName <- genJString
      dupFields <- listOfN(dupCount, genJField(Gen.const(dupName)))
      otherFields <- listOfN(fieldCount - dupCount, genJField(genJString))
      location <- genJLocation
    } yield JObject(Random.shuffle(dupFields ::: otherFields), Some(location))

  def getDuplicateFieldName(in: JObject): Option[String] =
    in.fieldList.groupBy(_.name.value).find(_._2.length > 1).map(_._1)

  val genNonEmptyJObject: Gen[JObject] = genJObject.retryUntil(_.fieldList.nonEmpty)
  val genEmptyJObject: Gen[JObject] = genJLocation.map(Some(_)).map(JObject(Nil, _))

  val genJArray: Gen[JArray] =
    for {
      len <- choose(0, 8)
      items <- listOfN(len, genJAny)
      location <- genJLocation
    } yield JArray(items, Some(location))

  val genNonEmptyJArray: Gen[JArray] = genJArray.retryUntil(_.items.nonEmpty)
  val genEmptyJArray: Gen[JArray] = genJLocation.map(Some(_)).map(JArray(Nil, _))

  def genJAny(strings: Gen[JString] = genJString): Gen[JAny] =
    frequency(
      20 -> strings,
      10 -> genJNumber,
      4 -> genJBoolean,
      4 -> genJNull,
      2 -> genJObject,
      1 -> genJArray,
    )

  val genJAny: Gen[JAny] = genJAny()

  def genJRootFocus[A <: JAny](genValue: Gen[A]): Gen[JRootFocus[A]] =
    for {
      v <- genValue
    } yield JRootFocus(v)

  /** Returns a deep focus on a generated value with the specified maximum depth. */
  def genJFocus[A <: JAny](gen: Gen[A], maxDepth: Int = 8): Gen[JFocus[A]] = {

    def nestInObject(focus: JFocus[A]): Gen[JFocus[A]] =
      for {
        obj <- genJObject
        key <- genJString
        index <- choose(0, obj.fieldList.size)
      } yield {
        val newRoot = obj.insert(index, JField(key, focus.root.value))
        val newPointer = JPointer(Key(key.value) :: focus.pointer.tokens)
        newRoot.asRootFocus.navigate(newPointer).getOrThrow.asInstanceOf[JFocus[A]]
      }

    def nestInArray(focus: JFocus[A]): Gen[JFocus[A]] =
      for {
        arr <- genJArray
        index <- choose(0, arr.items.size)
      } yield {
        val newRoot = arr.insert(index, focus.root.value)
        val newPointer = JPointer(Index(index) :: focus.pointer.tokens)
        newRoot.asRootFocus.navigate(newPointer).getOrThrow.asInstanceOf[JFocus[A]]
      }

    // Arbitrarily nest the specified root inside an object or array to the specified depth.
    def nest(focus: JFocus[A], remainingDepth: Int): Gen[JFocus[A]] =
      if (remainingDepth == 0)
        Gen.const(focus)
      else
        for {
          newParent <- oneOf(nestInObject(focus), nestInArray(focus))
          newRoot <- nest(newParent, remainingDepth - 1)
        } yield newRoot

    for {
      initialRoot <- gen
      depth <- choose(0, maxDepth)
      root <- nest(initialRoot.asRootFocus, depth)
    } yield root
  }

  implicit val arbJAny: Arbitrary[JAny] = Arbitrary(genJAny)
  implicit val arbJString: Arbitrary[JString] = Arbitrary(genJString)
  implicit val arbJNumber: Arbitrary[JNumber] = Arbitrary(genJNumber)
  implicit val arbJBoolean: Arbitrary[JBoolean] = Arbitrary(genJBoolean)
  implicit val arbJNull: Arbitrary[JNull] = Arbitrary(genJNull)
  implicit val arbJObject: Arbitrary[JObject] = Arbitrary(genJObject)
  implicit val arbJArray: Arbitrary[JArray] = Arbitrary(genJArray)
}
