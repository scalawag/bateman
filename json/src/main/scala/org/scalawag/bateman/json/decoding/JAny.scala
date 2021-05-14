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
import cats.syntax.validated._
import org.scalawag.bateman.json.{encoding, validIfEmpty}
import org.scalawag.bateman.json.decoding.parser.ParseResult

/** Represents the type of JSON value as metadata. */

sealed trait JType {
  val label: String
}

object JType {
  val all = List(JArray, JObject, JBoolean, JNumber, JString, JNull)
}

/** Represents a JSON value that was parsed from a source. All decoded values have information regarding their
  * provenance (line/column and source location, if specified). Subclasses of this hierarchy are geared towards
  * the decoding and extraction of data contained in incoming JSON documents.
  */

sealed trait JAny {

  /** The JSON type of this value. */
  val jType: JType

  /** The lexical position in the incoming JSON text where this value was located. */
  val location: JLocation

  /** The structural position in the incoming JSON text where this value was located. */
  val pointer: JPointer

  /** Calls a partial function with the JSON type of this value. When the type of this value is not handled by
    * the partial function, a JsonTypeMismatch is returned which indicates the types that would have been handled.
    * The partial function must handle at least one JSON type or an `IllegalArgumentException` is thrown.
    *
    * @param fn the partial function called with the type of this value.
    * @tparam A the output type of the partial function
    * @return the result of the partial function ''or'' a JsonTypeMismatch indicating the types that are handled
    */
  def forType[A](fn: PartialFunction[JType, DecodeResult[A]]): DecodeResult[A] = {
    def validTypes =
      NonEmptyChain
        .fromSeq(JType.all.filter(fn.isDefinedAt))
        .getOrElse(
          throw new IllegalArgumentException(
            "programmer error: forType partial function must be defined for at least one JType"
          )
        )
    def fail: PartialFunction[JType, DecodeResult[A]] = {
      case _ =>
        JsonTypeMismatch(this, validTypes).invalidNec
    }
    (fn orElse fail)(this.jType)
  }

  /** Attempts to narrow this JSON value to a JSON string.
    *
    * @return this value as a [[JString]] ''or'' a JsonTypeMismatch indicating that this value should be a JSON string
    */
  def asString: DecodeResult[JString] = JsonTypeMismatch(this, JString).invalidNec

  /** Attempts to narrow this JSON value to a JSON number.
    *
    * @return this value as a [[JNumber]] ''or'' a JsonTypeMismatch indicating that this value should be a JSON number
    */
  def asNumber: DecodeResult[JNumber] = JsonTypeMismatch(this, JNumber).invalidNec

  /** Attempts to narrow this JSON value to a JSON null.
    *
    * @return [[JNull]] ''or'' a JsonTypeMismatch indicating that this value should be JSON null
    */
  def asNull: DecodeResult[JNull] = JsonTypeMismatch(this, JNull).invalidNec

  /** Attempts to narrow this JSON value to a JSON object.
    *
    * @return this value as a [[JObject]] ''or'' a JsonTypeMismatch indicating that this value should be a JSON object
    */
  def asObject: DecodeResult[JObject] = JsonTypeMismatch(this, JObject).invalidNec

  /** Attempts to narrow this JSON value to a JSON array.
    *
    * @return this value as a [[JArray]] ''or'' a JsonTypeMismatch indicating that this value should be a JSON array
    */
  def asArray: DecodeResult[JArray] = JsonTypeMismatch(this, JArray).invalidNec

  /** Attempts to narrow this JSON value to a JSON boolean.
    *
    * @return this value as a [[JBoolean]] ''or'' a JsonTypeMismatch indicating that this value should be a JSON boolean
    */
  def asBoolean: DecodeResult[JBoolean] = JsonTypeMismatch(this, JBoolean).invalidNec

  /** Creates an encodable version of this JSON value. The encodable version strips out the provenance information,
    * so is better for doing position-independent comparisons of documents. The encodable version is also what you
    * need to modify the document for encoding as a JSON text.
    *
    * @return the encodable equivalent of this JSON value.
    */
  def toEncoding: encoding.JAny
}

/** Represents a JSON null value.
  *
  * @param location the lexical position of this value in the incoming JSON text
  * @param pointer the structural position of this value in the incoming JSON text
  */
final case class JNull(location: JLocation, pointer: JPointer) extends JAny {
  override val jType: JType = JNull
  override def asNull: DecodeResult[JNull] = this.validNec
  override def toEncoding: encoding.JNull = encoding.JNull
}

/** Represents the type of JSON null as metadata. */

case object JNull extends JType {
  override val label: String = "null"
}

/** Represents a JSON string value.
  *
  * @param value the [[String]] representation of this value
  * @param location the lexical position of this value in the incoming JSON text
  * @param pointer the structural position of this value in the incoming JSON text
  */

final case class JString(value: String, location: JLocation, pointer: JPointer) extends JAny {
  override val jType: JType = JString
  override def asString: DecodeResult[JString] = this.validNec
  override def toEncoding: encoding.JString = encoding.JString(value)
}

/** Represents the type of a JSON string as metadata. */

case object JString extends JType {
  override val label: String = "a string"

  /** Provides for pattern matching against a [[JString]] while ignoring its provenance. */
  def unapply(b: JString): Option[String] = Some(b.value)
}

/** Represents a JSON boolean value.
  *
  * @param value the [[Boolean]] representation of this value
  * @param location the lexical position of this value in the incoming JSON text
  * @param pointer the structural position of this value in the incoming JSON text
  */

final case class JBoolean(value: Boolean, location: JLocation, pointer: JPointer) extends JAny {
  override val jType: JType = JBoolean
  override def asBoolean: DecodeResult[JBoolean] = this.validNec
  override def toEncoding: encoding.JBoolean = encoding.JBoolean(value)
}

/** Represents the type of a JSON boolean as metadata. */

object JBoolean extends JType {
  override val label: String = "a boolean"

  /** Provides for pattern matching against a [[JBoolean]] while ignoring its provenance. */
  def unapply(b: JBoolean): Option[Boolean] = Some(b.value)
}

/** Represents a JSON number value. JSON numbers are represented internally as [[String]]s to preserve their precision
  * and format.
  *
  * @param value the [[String]] representation of this value
  * @param location the lexical position of this value in the incoming JSON text
  * @param pointer the structural position of this value in the incoming JSON text
  */

final case class JNumber(value: String, location: JLocation, pointer: JPointer) extends JAny {
  override val jType: JType = JNumber
  override def asNumber: DecodeResult[JNumber] = this.validNec
  override def toEncoding: encoding.JNumber = encoding.JNumber.unsafe(value)
}

/** Represents the type of a JSON number as metadata. */

case object JNumber extends JType {
  override val label: String = "a number"

  /** Provides for pattern matching against a [[JNumber]] while ignoring its provenance. */
  def unapply(n: JNumber): Option[String] = Some(n.value)
}

/** Represents a JSON array value.
  *
  * @param items the items contained in this array
  * @param location the lexical position of this value in the incoming JSON text
  * @param pointer the structural position of this value in the incoming JSON text
  */

final case class JArray(items: List[JAny], location: JLocation, pointer: JPointer) extends JAny {
  override val jType: JType = JArray
  override def asArray: DecodeResult[JArray] = this.validNec
  override def toEncoding: encoding.JArray = encoding.JArray(items.map(_.toEncoding): _*)

  def index(n: Int): Option[JAny] = items.drop(n).headOption

  def requiredIndex(n: Int): DecodeResult[JAny] =
    index(n).map(_.validNec).getOrElse {
      UnspecifiedIndex(this, n).invalidNec
    }
}

/** Represents the type of a JSON array as metadata. */

case object JArray extends JType {
  override val label: String = "an array"

  /** Provides for pattern matching against a [[JArray]] while ignoring its provenance. */
  def unapply(a: JArray): Option[List[JAny]] = Some(a.items)
}

/** Represents a single field within a JSON object ([[JObject]]).
  *
  * @param name the name of the field
  * @param value the value of the field
  */

final case class JField(name: JString, value: JAny)

/** Represents a JSON object value.
  *
  * @param fieldList the fields contained in this object in the original order from the JSON text
  * @param location the lexical position of this value in the incoming JSON text
  * @param pointer the structural position of this value in the incoming JSON text
  */

final case class JObject(
    fieldList: List[JField],
    location: JLocation,
    pointer: JPointer
) extends JAny {
  override val jType: JType = JObject
  override def asObject: DecodeResult[JObject] = this.validNec
  override def toEncoding: encoding.JObject =
    encoding.JObject(fieldList.map(f => f.name.value -> f.value.toEncoding): _*)

  /** Provides access to the fields as a map keyed off of the field's bare name. Contains DecodeErrors if there are
    * duplicate fields.
    */
  val fields: DecodeResult[Map[String, JField]] = {
    val fieldsByName = fieldList.groupBy(_.name.value)
    val duplicateSets = fieldsByName.collect { case (_, ff) if ff.size > 1 => ff.map(_.name) }
    val duplicateErrors = duplicateSets.flatMap { ff =>
      ff.tail.map(DuplicateField(_, ff.head))
    }
    validIfEmpty(duplicateErrors, fieldsByName.mapValues(_.head))
  }

  /** Retrieves the value of the field with the specified name, if it exists.
    *
    * @param fieldName the field name
    * @return the value of the field in a [[Some]] or, if it does not exist, [[None]]
    */
  def get(fieldName: String): DecodeResult[Option[JAny]] = fields.map(_.get(fieldName).map(_.value))

  /** Retrieves the value of the field with the specified name, if it exists.
    *
    * @param fieldName the field name
    * @return the value of the field ''or'' an [[UnspecifiedField]] error if it does not exist
    */
  def apply(fieldName: String): DecodeResult[JAny] =
    get(fieldName).andThen {
      case Some(v) => v.validNec
      case None    => UnspecifiedField(this, fieldName).invalidNec
    }
}

/** Represents the type of a JSON object as metadata. */

case object JObject extends JType {
  override val label: String = "an object"

  /** Provides for pattern matching against a [[JObject]] while ignoring its provenance. */
  def unapply(o: JObject): Option[List[JField]] = Some(o.fieldList)
}
