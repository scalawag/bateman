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

package org.scalawag.bateman.json

import cats.data.{NonEmptyChain, NonEmptySet}
import JErrorFormatters._
import cats.Order
import org.scalawag.bateman.json.focus.JFocus

trait JError {
  def location: Option[JLocation]
  def pointer: JPointer
  def description: String

  /** A description of the error including location and pointer information. */
  lazy val fullDescription: String = s"$pointer${optionalLocation(location)}: $description"
}

object JError {
  implicit val ordering: Ordering[JError] = Ordering.by(e => e.location -> e.pointer -> e.description)
  implicit val order: Order[JError] = Order.fromOrdering
}

/** Indicates that a value that was expected to exist in the JSON text did not exist. Errors of this type are "caught"
  * by code that only expected the value to exist optionally.
  */

trait MissingValue

/** Indicates that a JSON object was expected to have a field that was not there. In some cases, one of several fields
  * would satisfy the requirement, so all fields that would have sufficed are included.
  *
  * @param obj the object nearest the missing required field(s) that existed
  * @param names the list of pointers to the missing field, relative to [[obj]], that would have sufficed
  */

final case class MissingField(obj: JFocus[JObject], names: NonEmptyChain[String]) extends JError with MissingValue {
  override def location: Option[JLocation] = obj.value.location
  override def pointer: JPointer = obj.pointer
  override val description =
    s"JSON object is missing a required field: ${formatOrList(names.iterator)}"
}

object MissingField {

  /** Creates an [[MissingField]] error where there was only one field name that would suffice.
    *
    * @param obj the object that was missing a required field
    * @param field the field that is missing
    */
  def apply(obj: JFocus[JObject], field: String, fields: String*): MissingField =
    MissingField(obj, NonEmptyChain(field, fields: _*))
}

final case class MissingFieldIndex(obj: JFocus[JObject], index: Int) extends JError with MissingValue {
  override def location: Option[JLocation] = obj.value.location
  override def pointer: JPointer = obj.pointer
  override val description =
    s"JSON object has no field at index $index"
}

/** Indicates that a JSON array was expected to have an item at a particular index, but the array was too short to
  * contain that index.
  *
  * @param arr the array that was too short
  * @param index the index that was expected but not found
  */

final case class MissingIndex(arr: JFocus[JArray], index: Int) extends JError with MissingValue {
  override def location: Option[JLocation] = arr.value.location
  override def pointer: JPointer = arr.pointer
  override val description = s"JSON array is missing required value at index $index."
}

/** Indicates that a JSON value is not valid. What constitutes "valid" is determined by the code that creates this
  * error. There is no universal definition.
  *
  * @param value the value that is invalid
  * @param description the reason that it was deemed invalid
  */

final case class InvalidValue(value: JFocus[JAny], description: String) extends JError {
  override def location: Option[JLocation] = value.value.location
  override def pointer: JPointer = value.pointer
}

/** Indicates that a JSON value was found where it was not allowed. What values are allowed or disallowed is
  * determined solely by the code that creates this error.
  *
  * @param value the unexpected field
  */

final case class UnexpectedValue(value: JFocus[JAny]) extends JError {
  override def location: Option[JLocation] = value.value.location
  override def pointer: JPointer = value.pointer
  override val description = s"JSON value is not allowed here."
}

/** Indicates that a JSON object contained multiple fields with the same name. This is allowed by the JSON
  * specification.
  *
  * @param first the key of the duplicate field
  * @param next the key of the first field in the object with this name
  */

final case class DuplicateField(cur: JFocus[JObject], values: NonEmptyChain[JFocus[JAny]]) extends JError {
  override def location: Option[JLocation] = values.head.value.location
  override def pointer: JPointer = values.head.pointer // TODO: little weird. little different.
  // TODO: leave out the locations if they aren't available.
  override val description = s"JSON key '$pointer' has duplicate values (${values.iterator.flatMap(_.value.location)})."
}

/** Indicates that a JSON value was the wrong type. In some cases, several types are allowable. This error contains
  * all of the acceptable types, which is solely determined by the code creating this error.
  *
  * @param value the offending JSON value
  * @param expected the list of types that were expected here
  */
final case class JsonTypeMismatch(value: JFocus[JAny], expected: NonEmptySet[JType]) extends JError {
  override def location: Option[JLocation] = value.value.location
  override def pointer: JPointer = value.pointer
  override val description: String =
    s"Expecting ${formatOrList(expected.toNonEmptyList.iterator.map(_.label))} here, " +
      s"but ${value.value.jType.label} was found instead."
}

object JsonTypeMismatch {

  /** Create a [[JsonTypeMismatch]] indicating that only one JSON type was acceptable.
    *
    * @param value the offending JSON value
    * @param expected the expected JSON type
    */
  def apply(value: JFocus[JAny], expected: JType, expecteds: JType*): JsonTypeMismatch = {
//    throw new Exception("bad")
    apply(value, NonEmptySet.of(expected, expecteds: _*))
  }
}

final case class NoParent(value: JFocus[JAny]) extends JError {
  override def location: Option[JLocation] = value.value.location
  override def pointer: JPointer = value.pointer
  override val description = s"The root value has no parent."
}

/** Provides some consistency to the formatting of the error descriptions. */

trait JErrorFormatters {
  def optionalLocation(location: Option[JLocation]): String =
    location.map(x => s" ($x)").getOrElse("")

  def quote(s: String): String = s""""$s""""

  def formatOrList(items: Iterator[String]): String =
    formatList("or", items)

  def formatAndList(items: Iterator[String]): String =
    formatList("and", items)

  def formatList(
      conjunction: String,
      items: Iterator[String]
  ): String =
    items.toList match {
      case head :: Nil => head
      case itemList =>
        itemList
          .dropRight(1)
          .mkString("", ", ", s" $conjunction ") + itemList.last
    }

  def maybeBullets(lines: Iterable[String]): String =
    if (lines.size == 1) lines.head else lines.mkString(" - ", "\n - ", "")
}

object JErrorFormatters extends JErrorFormatters
