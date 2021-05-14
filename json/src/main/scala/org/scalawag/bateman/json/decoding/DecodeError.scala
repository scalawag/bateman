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

import cats.data.NonEmptyChain

trait DecodeError {
  def location: JLocation
  def pointer: JPointer
  def description: String

  /** A description of the error including location and pointer information. */
  def fullDescription: String = s"$pointer: $description ($location)"
}

object DecodeError {

  /** Creates a human-readable, sorted, deduplicated list of error messages. */
  def formatErrorReport(errors: NonEmptyChain[DecodeError]): String =
    errors.toChain.toList.sorted.distinct.map(_.fullDescription).mkString(" - ", "\n - ", "")

  implicit val ordering: Ordering[DecodeError] = Ordering.by(_.location)
}

/** Indicates that a value that was expected to exist in the JSON text did not exist. Errors of this type are "caught"
  * by code that only expected the value to exist optionally.
  */

trait MissingValue

/** Indicates that a JSON object was expected to have a field that was not there. In some cases, one of several fields
  * would satisfy the requirement, so all fields that would have sufficed are included.
  *
  * @param obj the object nearest the missing required field(s) that existed
  * @param pointers the list of pointers to the missing field, relative to [[obj]], that would have sufficed
  */

final case class UnspecifiedField(obj: JObject, pointers: NonEmptyChain[JPointer.Child])
    extends DecodeError
    with MissingValue
    with ErrorFormatters {
  override def location: JLocation = obj.location
  override def pointer: JPointer = obj.pointer
  override val description =
    s"JSON object at $pointer is missing a required field: ${formatOrList(pointers.iterator.map(_.toString))}."
  override def fullDescription: String = s"$description ($location)"
}

object UnspecifiedField {

  /** Creates an [[UnspecifiedField]] error where there was only one field name that would suffice.
    *
    * @param obj the object that was missing a required field
    * @param field the field that is missing
    */
  def apply(obj: JObject, field: String): UnspecifiedField =
    UnspecifiedField(obj, NonEmptyChain.one(JPointer.Root / field))

  def apply(obj: JObject, pointer: JPointer.Child): UnspecifiedField =
    UnspecifiedField(obj, NonEmptyChain.one(pointer))
}

/** Indicates that a JSON array was expected to have an item at a particular index, but the array was too short to
  * contain that index.
  *
  * @param arr the array that was too short
  * @param index the index that was expected but not found
  */

final case class UnspecifiedIndex(arr: JArray, index: Int) extends DecodeError with MissingValue with ErrorFormatters {
  override def location: JLocation = arr.location
  override def pointer: JPointer = arr.pointer
  override val description = s"JSON array is missing required value at index $index."
}

/** Indicates that a JSON value is not valid. What constitutes "valid" is determined by the code that creates this
  * error. There is no universal definition.
  *
  * @param value the value that is invalid
  * @param description the reason that it was deemed invalid
  */

final case class InvalidValue(value: JAny, description: String) extends DecodeError {
  override def location: JLocation = value.location
  override def pointer: JPointer = value.pointer
}

/** Indicates that a JSON value was found where it was not allowed. What values are allowed or disallowed is
  * determined solely by the code that creates this error.
  *
  * @param value the unexpected field
  */

final case class UnexpectedValue(value: JAny) extends DecodeError {
  override def location: JLocation = value.location
  override def pointer: JPointer = value.pointer
  override val description = s"JSON value is not allowed here."
}

/** Indicates that a JSON object contained multiple fields with the same name. This is allowed by the JSON
  * specification.
  *
  * @param key the key of the duplicate field
  * @param first the key of the first field in the object with this name
  */

final case class DuplicateField(key: JString, first: JString) extends DecodeError {
  override def location: JLocation = key.location
  override def pointer: JPointer = key.pointer
  override val description = s"JSON key '${key.value}' duplicates another one at ${first.location}."
}

/** Indicates that a JSON value was the wrong type. In some cases, several types are allowable. This error contains
  * all of the acceptable types, which is solely determined by the code creating this error.
  *
  * @param value the offending JSON value
  * @param expected the list of types that were expected here
  */
final case class JsonTypeMismatch(value: JAny, expected: NonEmptyChain[JType])
    extends DecodeError
    with ErrorFormatters {
  override def location: JLocation = value.location
  override def pointer: JPointer = value.pointer
  override val description: String =
    s"Expecting ${formatOrList(expected.iterator.map(_.label))} here, " +
      s"but ${value.jType.label} was found instead."
}

object JsonTypeMismatch {

  /** Create a [[JsonTypeMismatch]] indicating that only one JSON type was acceptable.
    *
    * @param value the offending JSON value
    * @param expected the expected JSON type
    */
  def apply(value: JAny, expected: JType): JsonTypeMismatch = apply(value, NonEmptyChain(expected))
}

/** Provides some consistency to the formatting of the error descriptions. */

trait ErrorFormatters {
  protected def quote(s: String): String = s""""$s""""

  protected def formatOrList(items: Iterator[String]): String =
    formatList("or", items)

  protected def formatAndList(items: Iterator[String]): String =
    formatList("and", items)

  protected def formatList(
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
}
