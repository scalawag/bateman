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

import cats.{Monoid, Order}
import cats.syntax.either._
import org.scalawag.bateman.json.JNull.JNullImpl
import org.scalawag.bateman.json.focus.JRootFocus

import scala.annotation.tailrec

/** Represents the type of JSON value as metadata. */

sealed trait JType {
  val label: String
}

object JType {
  def apply[A <: JAny](implicit summoner: Summoner[A]): JType = summoner()

  trait Summoner[A] {
    def apply(): JType
  }

  implicit val jtypeForJArray: Summoner[JArray] = () => JArray
  implicit val jtypeForJObject: Summoner[JObject] = () => JObject
  implicit val jtypeForJBoolean: Summoner[JBoolean] = () => JBoolean
  implicit val jtypeForJNumber: Summoner[JNumber] = () => JNumber
  implicit val jtypeForJString: Summoner[JString] = () => JString
  implicit val jtypeForJNull: Summoner[JNull] = () => JNull

  implicit val ordering: Ordering[JType] = Ordering.by(_.toString)
  implicit val order: Order[JType] = Order.fromOrdering
}

/** Represents a JSON value that was parsed from a source. All decoded values have information regarding their
  * provenance (line/column and source location, if specified). Subclasses of this hierarchy are geared towards
  * the decoding and extraction of data contained in incoming JSON documents.
  */

sealed trait JAny {

  /** The JSON type of this value. */
  val jType: JType

  /** The lexical position in the incoming JSON text where this value was located. This should only exist for
    * values that have come from parsing a JSON text. Values created in the code have no location. Once values
    * that ''do'' have location are modified in any way, the location information is stripped from them.
    *
    * Absence of location information indicates that none of the children of this value have location information
    * either.
    */
  val location: Option[JLocation]

  def render: String = render(NoSpacesRenderer)
  def spaces2: String = render(PrettySpaces2)
  def spaces4: String = render(PrettySpaces4)
  def render(renderer: Renderer): String = renderer.render(this)
}

object JAny {
  implicit class JAnyOps[A <: JAny](me: A) {

    /** Arbitrarily creates a root focus from this JSON value for the purposes of navigating, extracting and
      * transforming. You may have to do this for JSON values that you create programmatically. Parsing a JSON text
      * returns a focus, which can be used directly for the aforementioned activities without calling this method.
      */

    def asRootFocus: JRootFocus[A] =
      JRootFocus(me)

    def stripLocation: A = {
      if (me.location.isEmpty)
        me
      else
        me match {
          case in: JString  => in.copy(location = None)
          case in: JNumber  => in.copy(location = None)
          case in: JBoolean => in.copy(location = None)
          case in: JNull    => JNull
          case in: JArray   => in.copy(items = in.items.map(_.stripLocation), location = None)
          case in: JObject  => in.copy(fieldList = in.fieldList.map(_.stripLocation), location = None)
        }
    }.asInstanceOf[A]
  }
}

/** Represents a JSON null value. */
sealed trait JNull extends JAny

/** Represents the type of JSON null as metadata. */

case object JNull extends JType with JNull {
  override val label: String = "null"
  override val jType: JType = this
  override val location: Option[JLocation] = None

  /** Represents a JSON null value.
    *
    * @param location the lexical position of this value in the incoming JSON text (if applicable)
    */
  final case class JNullImpl private[bateman] (location: Option[JLocation] = None) extends JNull {
    override val jType: JType = JNull
  }
}

/** Represents a JSON string value.
  *
  * @param value the [[String]] representation of this value
  * @param location the lexical position of this value in the incoming JSON text (if applicable)
  */

final case class JString private[bateman] (value: String, location: Option[JLocation]) extends JAny {
  override val jType: JType = JString
}

/** Represents the type of a JSON string as metadata. */

case object JString extends JType {
  override val label: String = "a string"

  def apply(value: String): JString = JString(value, None)

  object Value {

    /** Provides for pattern matching against a [[JString]] while ignoring its location. */
    def unapply(b: JString): Option[String] = Some(b.value)
  }
}

/** Represents a JSON boolean value.
  *
  * @param value the [[Boolean]] representation of this value
  * @param location the lexical position of this value in the incoming JSON text (if applicable)
  */

final case class JBoolean private[bateman] (value: Boolean, location: Option[JLocation]) extends JAny {
  override val jType: JType = JBoolean
}

/** Represents the type of a JSON boolean as metadata. */

case object JBoolean extends JType {
  override val label: String = "a boolean"

  def apply(value: Boolean): JBoolean = JBoolean(value, None)

  object Value {

    /** Provides for pattern matching against a [[JBoolean]] while ignoring its location. */
    def unapply(b: JBoolean): Option[Boolean] = Some(b.value)
  }
}

/** Represents a JSON number value. JSON numbers are represented internally as [[Predef.String String]]s to preserve
  * their precision and format.
  *
  * @param value the [[String]] representation of this value
  * @param location the lexical position of this value in the incoming JSON text (if applicable)
  */

final case class JNumber private[json] (value: String, location: Option[JLocation]) extends JAny {
  override val jType: JType = JNumber

  lazy val toBigDecimal: BigDecimal = BigDecimal(value)
}

/** Represents the type of a JSON number as metadata. */

case object JNumber extends JType {
  override val label: String = "a number"

  private val re = """-?(?:0|[1-9]\d*)(?:\.\d+)?(?:[eE][+-]?\d+)?""".r

  def apply(n: String): Either[String, JNumber] =
    n match {
      case re() => Right(new JNumber(n, None))
      case _    => Left(s"'$n' is not a valid JSON number")
    }

  def unsafe(n: String): JNumber = JNumber(n).fold(e => throw new IllegalArgumentException(e), identity)
  def apply(l: Long): JNumber = unsafe(l.toString)
  def apply(i: BigInt): JNumber = unsafe(i.toString)
  def apply(d: BigDecimal): JNumber = unsafe(d.toString)

  object Value {

    /** Provides for pattern matching against a [[JNumber]] while ignoring its location. */
    def unapply(n: JNumber): Option[String] = Some(n.value)
  }
}

/** Represents a JSON array value.
  *
  * @param items the items contained in this array
  * @param location the lexical position of this value in the incoming JSON text (if applicable)
  */

final case class JArray private[bateman] (items: List[JAny], location: Option[JLocation]) extends JAny {
  override val jType: JType = JArray

  def length: Int =
    items.length

  /** Throws IndexOutOfBoundException is the index is out of bounds. */
  def updated(index: Int, value: JAny): JArray =
    JArray(items.map(_.stripLocation).updated(index, value.stripLocation), None)

  def delete(index: Int): JArray =
    JArray(items = items.take(index).map(_.stripLocation) ::: items.drop(index + 1).map(_.stripLocation), None)

  def insert(index: Int, item: JAny): JArray =
    JArray(
      items = items.take(index).map(_.stripLocation) ::: item.stripLocation :: items.drop(index).map(_.stripLocation),
      None
    )

  def prepend(item: JAny): JArray =
    insert(0, item)

  def append(item: JAny): JArray =
    insert(length, item)

  def ++(that: JArray): JArray =
    JArray(this.items.map(_.stripLocation) ++ that.items.map(_.stripLocation), None)
}

/** Represents the type of a JSON array as metadata. */

case object JArray extends JType {
  override val label: String = "an array"

  val Empty: JArray = JArray(Nil, None)

  def apply(items: JAny*): JArray = new JArray(items.toList.map(_.stripLocation), None)

  implicit val monoidForJArray: Monoid[JArray] = new Monoid[JArray] {
    override def empty: JArray = Empty
    // This wackiness is due to the laws (and maintaining location).
    override def combine(x: JArray, y: JArray): JArray =
      // Only wipe out the location info if both inputs are non-empty
      if (x == Empty) y
      else if (y == Empty) x
      else if (x.items.isEmpty) y
      else if (y.items.isEmpty) x
      else x ++ y
  }

  object Value {

    /** Provides for pattern matching against a [[JArray]] while ignoring its provenance. */
    def unapply(a: JArray): Option[List[JAny]] = Some(a.items)
  }

  object Values {

    /** Note that this won't work for arrays with exactly one  */
    def unapplySeq(a: JArray): Option[Seq[JAny]] = Some(a.items)
  }
}

/** Represents a single field within a JSON object ([[JObject]]).
  *
  * @param name the name of the field
  * @param value the value of the field
  */

final case class JField(name: JString, value: JAny) {
  def stripLocation: JField = JField(name.stripLocation, value.stripLocation)
}

/** Represents a JSON object value.
  *
  * @param fieldList the fields contained in this object in the original order from the JSON text
  * @param location the lexical position of this value in the incoming JSON text (if applicable)
  */

final case class JObject private[bateman] (fieldList: List[JField], location: Option[JLocation]) extends JAny {
  override val jType: JType = JObject

  /** Throws IndexOutOfBoundException is the index is out of bounds. */
  def updated(index: Int, value: JAny): JObject = {
    val f = fieldList.apply(index).copy(value = value.stripLocation).stripLocation
    JObject(fieldList.take(index).map(_.stripLocation) ::: f :: fieldList.drop(index + 1).map(_.stripLocation), None)
  }

  def delete(index: Int): JObject =
    JObject(fieldList.take(index).map(_.stripLocation) ::: fieldList.drop(index + 1).map(_.stripLocation), None)

  def insert(index: Int, field: JField): JObject =
    JObject(
      fieldList.take(index).map(_.stripLocation) ::: field.stripLocation :: fieldList.drop(index).map(_.stripLocation),
      None
    )

  def insert(index: Int, name: String, value: JAny): JObject =
    insert(index, JField(JString(name), value))

  def prepend(name: String, value: JAny): JObject =
    insert(0, name, value)

  def append(name: String, value: JAny): JObject =
    insert(fieldList.length, name, value)

  def ++(that: JObject): JObject =
    JObject(this.fieldList.map(_.stripLocation) ++ that.fieldList.map(_.stripLocation), None)
}

/** Represents the type of a JSON object as metadata. */

case object JObject extends JType {
  override val label: String = "an object"

  val Empty: JObject = JObject(Nil, None)

  def apply(fields: (String, JAny)*): JObject =
    new JObject(
      fields.map {
        case (l, v) =>
          JField(JString(l), v)
      }.toList,
      None
    )

  def flatten(fields: Iterable[(String, JAny)]*): JObject = apply(fields.flatten: _*)

  implicit val monoidForJObject: Monoid[JObject] = new Monoid[JObject] {
    override def empty: JObject = JObject.Empty
    // This wackiness is due to the laws (and maintaining location).
    override def combine(x: JObject, y: JObject): JObject =
      // Only wipe out the location info if both inputs are non-empty
      if (x == Empty) y
      else if (y == Empty) x
      else if (x.fieldList.isEmpty) y
      else if (y.fieldList.isEmpty) x
      else x ++ y
  }
}
