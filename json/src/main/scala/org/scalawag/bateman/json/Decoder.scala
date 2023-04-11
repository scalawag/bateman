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

import cats.Functor
import cats.syntax.apply._
import cats.syntax.functor._
import cats.syntax.parallel._
import cats.syntax.either._
import org.scalawag.bateman.json.focus.{JFieldFocus, JFocus}
import org.scalawag.bateman.json.validating.Validator
import org.scalawag.bateman.json.focus.weak._

import java.time.{Instant, LocalDate, LocalDateTime, LocalTime}
import java.util.UUID
import scala.reflect.{ClassTag, classTag}

/** Provides the ability to decode one object to another.
  *
  * Decoder is intentionally invariant in the input and output types so that we can better control what decoders are
  * used. There are are narrowing decoders implicitly available that effectively make the input type covariant
  * if there is not a more specific decoder available.
  *
  * @tparam In the input type for the decoder
  * @tparam Out the output type of the decoder
  */

trait Decoder[In <: JAny, Out] { self =>

  /** Attempts to decode a given input to its corresponding output.
    *
    * @param in the input to decode
    * @return the decoded output, if possible, otherwise an invalid result containing the errors
    */
  def decode(in: JFocus[In]): JResult[Out]
}

object Decoder extends DecoderLowP {

  /** Summons a specifed decoder (which must already exist in implicit scope) by type. */
  def apply[In <: JAny, Out](implicit decoder: Decoder[In, Out]): Decoder[In, Out] = decoder

  /** Decodes something to itself. This always succeeds. */
  implicit def identityDecoder[FromTo <: JAny]: Decoder[FromTo, FromTo] = _.value.rightNec

  /** Simply decodes a JSON string as a Scala String. */
  implicit val stringDecoder: JStringDecoder[String] = _.value.value.rightNec

  /** Simply decodes a JSON boolean as a Scala Boolean. */
  implicit val booleanDecoder: JBooleanDecoder[Boolean] = _.value.value.rightNec

  /** Creates a decoder that converts a JSON value into another type using a transformation function.
    * If a [[RuntimeException]] is thrown during the conversion, it is returned as an [[InvalidValue]] error.
    *
    * @param fn converts the input type to the output type, may throw [[RuntimeException]]s
    * @tparam In  the input type of the decoder
    * @tparam Out the output type of the decoder
    * @return a new decoder that decodes [[In]] to [[Out]]
    */
  def unsafe[In <: JAny, Out](fn: In => Out): Decoder[In, Out] = { in =>
    try {
      fn(in.value).rightNec
    } catch {
      case ex: RuntimeException => InvalidValue(in, ex.getMessage).leftNec
    }
  }

  private def unsafeNumber[Out: ClassTag](fn: String => Out): JNumberDecoder[Out] = { in =>
    val s = in.value.value
    try {
      fn(s).rightNec
    } catch {
      case _: NumberFormatException =>
        InvalidValue(in, s"'$s' is not a valid ${classTag[Out]}").leftNec
    }
  }

  implicit val jnumberToByteDecoder: JNumberDecoder[Byte] = unsafeNumber(_.toByte)
  implicit val jnumberToShortDecoder: JNumberDecoder[Short] = unsafeNumber(_.toShort)
  implicit val jnumberToIntDecoder: JNumberDecoder[Int] = unsafeNumber(_.toInt)
  implicit val jnumberToLongDecoder: JNumberDecoder[Long] = unsafeNumber(_.toLong)
  implicit val jnumberToFloatDecoder: JNumberDecoder[Float] = unsafeNumber(_.toFloat)
  implicit val jnumberToDoubleDecoder: JNumberDecoder[Double] = unsafeNumber(_.toDouble)
  implicit val jnumberToBigIntDecoder: JNumberDecoder[BigInt] = unsafeNumber(BigInt(_))
  implicit val jnumberToBigDecDecoder: JNumberDecoder[BigDecimal] = unsafeNumber(BigDecimal.exact)

  private def unsafeString[Out: ClassTag](fn: String => Out, format: String): JStringDecoder[Out] = { in =>
    val s = in.value.value
    try {
      fn(s).rightNec
    } catch {
      case _: RuntimeException =>
        val cls = classTag[Out].runtimeClass.getSimpleName
        InvalidValue(in, s"'$s' is not a valid $cls (must be in $format format)").leftNec
    }
  }

  implicit val localDateDecoder: JStringDecoder[LocalDate] =
    unsafeString(LocalDate.parse, "'yyyy-mm-dd'")
  implicit val localTimeDecoder: JStringDecoder[LocalTime] =
    unsafeString(LocalTime.parse, "'hh:mm:ss'")
  implicit val localDateTimeDecoder: JStringDecoder[LocalDateTime] =
    unsafeString(LocalDateTime.parse, "'yyyy-mm-ddThh:mm:ss'")
  implicit val instantDecoder: JStringDecoder[Instant] =
    unsafeString(Instant.parse, "'yyyy-mm-ddThh:mm:ss[.sss[sss[sss]]]Z'")

  implicit val uuidDecoder: JStringDecoder[UUID] =
    unsafeString(UUID.fromString, "8-4-4-4-12 hex")

  /** Decodes a JSON string as a character. Returns an InvalidValue error if the input string is not exactly one
    * character long.
    */
  implicit val charDecoder: JStringDecoder[Char] = { in =>
    if (in.value.value.length == 1)
      in.value.value.head.rightNec
    else
      InvalidValue(in, s"'${in.value.value}' is not a string of exactly length one").leftNec
  }

  /** Decodes something nullable from a JSON value. JSON null is always decoded as Null. Any other JSON value is
    * decoded using the underlying decoder (which may fail).
    *
    * @param dec the underlying decode which handles non-null JSON values
    * @tparam To the output type of the underlying decoder
    * @return the new decoder
    */

  implicit def nullableDecoder[To](implicit
      dec: JAnyDecoder[To]
  ): JAnyDecoder[Nullable[To]] =
    cur =>
      cur.asNull match {
        case Right(in) =>
          Null.rightNec
        case Left(_) =>
          dec
            .decode(cur)
            .fold(
              // We need to amend any JsonTypeMismatch errors that occurred on this value to indicate that
              // JNull would also have been supported.
              _.map {
                case JsonTypeMismatch(v, expected) if v == cur && !expected.contains(JNull) =>
                  JsonTypeMismatch(v, expected.add(JNull))
                case e => e
              }.asLeft,
              x => NotNull(x).rightNec
            )

      }

  /** Decodes a map from a JSON object. It defers to existing decoders to handle the keys and values.
    *
    * @param keyDecoder   a JString decoder used to decode the keys
    * @param valueDecoder a JAny decoder used to decode the values
    * @tparam K the type of the keys in the output map
    * @tparam V the type of the values in the output map
    * @return the new decoder
    */

  implicit def mapDecoder[K, V](implicit
      keyDecoder: JStringDecoder[K],
      valueDecoder: JAnyDecoder[V]
  ): Decoder[JObject, Map[K, V]] = pairsDecoder[K, V].map(_.toMap)

  /** Decodes key/value pairs from a JSON object. It defers to existing decoders to handle the keys and values.
    * Use this instead of the [[mapDecoder]] if you want to preserve the ordering from the JSON object.
    *
    * @param keyDecoder   a JString decoder used to decode the keys
    * @param valueDecoder a JAny decoder used to decode the values
    * @tparam K the type of the keys in the output map
    * @tparam V the type of the values in the output map
    * @return the new decoder
    */

  implicit def pairsDecoder[K, V](implicit
      keyDecoder: JStringDecoder[K],
      valueDecoder: JAnyDecoder[V]
  ): Decoder[JObject, Seq[(K, V)]] = { objCursor =>
    objCursor.fields
      .parTraverse { valueFocus =>
        val keyFocus = valueFocus match {
          case f: JFieldFocus[_, _] => JFieldFocus(f.name, f.name, f.index, f.parent)
          case _                    => ???
        }
        (keyDecoder.decode(keyFocus), valueDecoder.decode(valueFocus)).tupled.map {
          case (kCursor, vCursor) =>
            kCursor -> vCursor
        }
      }
  }

  /** Decodes a list from a JSON array. It defers to an underlying decoder to handle the items.
    *
    * @param itemDecoder a JAny decoder used to decode the items
    * @tparam A the type of the items in the output list
    * @return the new decoder
    */
  implicit def listDecoder[A](implicit
      itemDecoder: JAnyDecoder[A]
  ): JArrayDecoder[List[A]] = { arrCursor =>
    arrCursor.items.parTraverse(itemDecoder.decode)
  }

  /** Adds additional capabilities to any decoder whose input type is a JSON value. */
  implicit class DecoderOps[In <: JAny, Out](d: Decoder[In, Out]) {

    /** Creates a new decoder, based on this decoder, which validates the output using a validator. This can be used
      * for semantic validation beyond what is automatically done (type mismatch, etc.).
      *
      * @param validator the validator used to validate the output of this decoder
      *                  and produce the output of the new decoder
      * @tparam NewOut the output type of the new decoder
      * @return the new decoder
      */
    def withValidation[NewOut](implicit validator: Validator[Out, NewOut]): Decoder[In, NewOut] = { cur =>
      d.decode(cur).flatMap { b =>
        validator
          .validate(b)
          .leftMap(ee => ee.map(e => InvalidValue(cur, e.fullDescription)))
      }
    }
  }

  implicit class JAnyToJAnyDecoderOps[In <: JAny, Out <: JAny](me: Decoder[In, Out]) {
    def andThen[Out2](that: Decoder[Out, Out2]): Decoder[In, Out2] =
      in => me.decode(in).map(in.as).flatMap(that.decode)
  }

  /** Creates a decoder that turns a JSON string into a numeric type.
    *
    * This behavior must be explicitly requested, otherwise it is ambiguous what to do when a [[JAny]] needs to be
    * decoded to a numeric type. There are two paths: one from a JSON number and one from a JSON string.
    *
    * @param decoder a decode from a JNumber to the output type
    * @tparam Out the output type of the new decoder
    * @return a new decoder that decodes from a JSON string to a numeric type [[Out]]
    */

  val jstringToJNumber: JStringDecoder[JNumber] = { in =>
    JNumber(in.value.value).fold(e => InvalidValue(in, e).leftNec, _.rightNec)
  }

  implicit def functorForDecoder[A <: JAny]: Functor[Decoder[A, *]] =
    new Functor[Decoder[A, *]] {
      override def map[B, C](fa: Decoder[A, B])(f: B => C): Decoder[A, C] = b => fa.decode(b).map(f)
    }

}

trait DecoderLowP extends DecoderLowLowP {
  // Decoder is invariant in the input type so that we can better control it. These serve to widen a Decoder with
  // a specific input type into a JAnyDecoder that will fail with a JsonTypeMismatch if the input is not the
  // expected type.

  /** Creates a decoder that will attempt to narrow a [[JAny]] to a [[JNull]] where there is no specific [[JAny]]
    * decoder available. It will fail with a JsonTypeMismatch if it is not the correct JSON type. If it is the correct
    * JSON type, it will defer to the underlying decoder.
    */
  implicit def widenJNullDecoder[A](implicit dec: Decoder[JNull, A]): JAnyDecoder[A] = widenDecoder(_.asNull)

  /** Creates a decoder that will attempt to narrow a [[JAny]] to a [[JNumber]] where there is no specific [[JAny]]
    * decoder available. It will fail with a JsonTypeMismatch if it is not the correct JSON type. If it is the correct
    * JSON type, it will defer to the underlying decoder.
    */
  implicit def widenJNumberDecoder[A](implicit dec: JNumberDecoder[A]): JAnyDecoder[A] = widenDecoder(_.asNumber)

  /** Creates a decoder that will attempt to narrow a [[JAny]] to a [[JBoolean]] where there is no specific [[JAny]]
    * decoder available. It will fail with a JsonTypeMismatch if it is not the correct JSON type. If it is the correct
    * JSON type, it will defer to the underlying decoder.
    */
  implicit def widenJBooleanDecoder[A](implicit dec: JBooleanDecoder[A]): JAnyDecoder[A] = widenDecoder(_.asBoolean)

  /** Creates a decoder that will attempt to narrow a [[JAny]] to a [[JObject]] where there is no specific [[JAny]]
    * decoder available. It will fail with a JsonTypeMismatch if it is not the correct JSON type. If it is the correct
    * JSON type, it will defer to the underlying decoder.
    */
  implicit def widenJObjectDecoder[A](implicit dec: Decoder[JObject, A]): JAnyDecoder[A] = widenDecoder(_.asObject)

  /** Creates a decoder that will attempt to narrow a [[JAny]] to a [[JArray]] where there is no specific [[JAny]]
    * decoder available. It will fail with a JsonTypeMismatch if it is not the correct JSON type. If it is the correct
    * JSON type, it will defer to the underlying decoder.
    */
  implicit def widenJArrayDecoder[A](implicit dec: JArrayDecoder[A]): JAnyDecoder[A] = widenDecoder(_.asArray)
}

trait DecoderLowLowP {
  protected def widenDecoder[A <: JAny, B](
      narrower: JFocus[JAny] => JResult[JFocus[A]]
  )(implicit decoder: Decoder[A, B]): JAnyDecoder[B] = narrower(_).flatMap(decoder.decode)

  /** Creates a decoder that will attempt to narrow a [[JAny]] to a [[JString]] where there is no specific [[JAny]]
    * decoder available. It will fail with a JsonTypeMismatch if it is not the correct JSON type. If it is the correct
    * JSON type, it will defer to the underlying decoder.
    *
    * Needs to be lower priority than JNumber to avoid ambiguity.
    */
  implicit def widenJStringDecoder[A](implicit dec: JStringDecoder[A]): JAnyDecoder[A] = widenDecoder(_.asString)
}
