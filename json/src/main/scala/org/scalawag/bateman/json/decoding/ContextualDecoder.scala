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

import cats.Eq
import cats.data.NonEmptyChain
import cats.syntax.apply._
import cats.syntax.traverse._
import cats.syntax.validated._
import org.scalawag.bateman.json.{NotNull, Null, Nullable}
import org.scalawag.bateman.json.validating.Validator

import java.time.{Instant, LocalDate, LocalDateTime, LocalTime}
import java.util.UUID

/** Provides the ability to decode one object to another within a certain context.
  *
  * Decoder is intentionally invariant in the input and output types so that we can better control what decoders are
  * used. There are are narrowing decoders implicitly available that effectively make the input type covariant
  * if there is not a more specific decoder available.
  *
  * @tparam In the input type for the decoder
  * @tparam Out the output type of the decoder
  * @tparam Context the context type within which this decoder can operate
  */

trait ContextualDecoder[In, Out, -Context] {

  /** Attempts to decode a given input to its corresponding output.
    *
    * @param in the input to decode
    * @param context the context within which to decode the input
    * @return the decoded output, if possible, otherwise an invalid result containing the errors
    */
  def decode(in: In, context: Context): DecodeResult[Out]

  /** Creates a new decoder, based on this decoder, that transforms its output in a way that can't fail.
    *
    * @param fn the transformation from the output of this decoder to the output of the new decoder
    * @tparam NewOut the output type of the new decoder
    * @return the new decoder
    */
  def map[NewOut](fn: Out => NewOut): ContextualDecoder[In, NewOut, Context] =
    decode(_, _).map(fn)

  /** Creates a new decoder, based on this decoder, that transforms its output in a way that can fail.
    *
    * @param fn the transformation from the output of this decoder to the output of the new decoder
    * @tparam NewOut the output type of the new decoder
    * @return the new decoder
    */
  def andThen[NewOut](fn: Out => DecodeResult[NewOut]): ContextualDecoder[In, NewOut, Context] =
    decode(_, _).andThen(fn)
}

object ContextualDecoder {

  /** Summons a specifed decoder (which must already exist in implicit scope) by type. */
  def apply[In, Out, Context](implicit
      decoder: ContextualDecoder[In, Out, Context]
  ): ContextualDecoder[In, Out, Context] =
    decoder

  /** Decodes something to itself in any context. This always succeeds. */
  implicit def identityDecoder[FromTo]: Decoder[FromTo, FromTo] = Decoder(_.validNec)

  /** Decodes something nullable from a JSON value. JSON null is always decoded as Null. Any other JSON value is
    * decoded using the underlying decoder (which may fail).
    *
    * @param dec the underlying decode which handles non-null JSON values
    * @tparam To the output type of the underlying decoder
    * @tparam Context the context in which this decoder operates
    * @return the new decoder
    */

  implicit def nullableDecoder[To, Context](implicit
      dec: ContextualDecoder[JAny, To, Context]
  ): ContextualDecoder[JAny, Nullable[To], Context] = {
    case (in: JNull, _) =>
      Null(in).validNec
    case (in, context) =>
      dec.decode(in, context).map(NotNull.apply).leftMap { ee =>
        // We need to amend any JsonTypeMismatch errors that occurred on this value to indicate that JNull would also
        // have been supported.
        ee.map {
          case JsonTypeMismatch(v, expected) if v == in && !expected.contains(JNull)(Eq.fromUniversalEquals) =>
            JsonTypeMismatch(v, expected ++ NonEmptyChain.one(JNull))
          case e => e
        }
      }
  }

  /** Decodes a map from a JSON object. It defers to existing decoders to handle the keys and values.
    *
    * @param keyDecoder a JString decoder that operates in this context used to decode the keys
    * @param valueDecoder a JAny decoder that operates in this context used to decode the values
    * @tparam K the type of the keys in the output map
    * @tparam V the type of the values in the output map
    * @tparam Context the type of the context in which this decoder operates
    * @return the new decoder
    */

  implicit def mapDecoder[K, V, Context](implicit
      keyDecoder: ContextualDecoder[JString, K, Context],
      valueDecoder: ContextualDecoder[JAny, V, Context]
  ): ContextualDecoder[JObject, Map[K, V], Context] = { (from, context) =>
    from.fieldList
      .traverse(f => (keyDecoder.decode(f.name, context), valueDecoder.decode(f.value, context)).tupled)
      .map(_.toMap)
  }

  /** Decodes a list from a JSON array. It defers to an underlying decoder to handle the items.
    *
    * @param itemDecoder a JAny decoder that operates in this context used to decode the items
    * @tparam A the type of the items in the output list
    * @tparam Context the type of the context in which this decoder operates
    * @return the new decoder
    */
  implicit def listDecoder[A, Context](implicit
      itemDecoder: ContextualDecoder[JAny, A, Context]
  ): ContextualDecoder[JArray, List[A], Context] = { (from, context) =>
    from.items.traverse(itemDecoder.decode(_, context))
  }

  /** Adds additional capabilities to any decoder whose input type is a JSON value. */
  implicit class JAnyDecoderOps[In <: JAny, Out, Context](d: ContextualDecoder[In, Out, Context]) {

    /** Creates a new decoder, based on this decoder, which validates the output using a validator. This can be used
      * for semantic validation beyond what is automatically done (type mismatch, etc.).
      *
      * @param validator the validator used to validate the output of this decoder
      *                  and produce the output of the new decoder
      * @tparam NewOut the output type of the new decoder
      * @return the new decoder
      */
    def withValidation[NewOut](implicit validator: Validator[Out, NewOut]): ContextualDecoder[In, NewOut, Context] = {
      (in, context) =>
        d.decode(in, context).andThen { b =>
          validator.validate(b).leftMap(ee => ee.map(e => InvalidValue(in, e.fullDescription)))
        }
    }
  }

  /** Adds additional operations to any decoder that does not depend on its context (i.e, has a context of [[Any]]). */

  implicit class ContextFreeDecoderOps[In, Out](d: Decoder[In, Out]) {
    def decode(from: In): DecodeResult[Out] = d.decode(from, ())
  }

  //  def wideDecoder[From, To, WideFrom >: From, Context](implicit
//      dec: ContextualDecoder[WideFrom, To, Context]
//  ): ContextualDecoder[From, To, Context] = dec.decode(_, _)

  implicit val jnullDecoder: Decoder[JAny, JNull] = Decoder(_.asNull)
  implicit val jarrayDecoder: Decoder[JAny, JArray] = Decoder(_.asArray)
  implicit val jobjectDecoder: Decoder[JAny, JObject] = Decoder(_.asObject)
  implicit val jstringDecoder: Decoder[JAny, JString] = Decoder(_.asString)
  implicit val jnumberDecoder: Decoder[JAny, JNumber] = Decoder(_.asNumber)
  implicit val jbooleanDecoder: Decoder[JAny, JBoolean] = Decoder(_.asBoolean)

  implicit val stringDecoder: Decoder[JString, String] = Decoder(_.value.validNec)

  implicit val booleanDecoder: Decoder[JBoolean, Boolean] = Decoder(_.value.validNec)

  /** Decodes a JSON string as a character. Returns an InvalidValue error if the input string is not exactly one
    * character long.
    */
  implicit val charDecoder: Decoder[JString, Char] =
    Decoder { s =>
      if (s.value.length == 1)
        s.value.head.validNec
      else
        InvalidValue(s, "expecting a one-character-long string").invalidNec
    }

  /** Creates a decoder that converts a value into another using a transformation function. If a [[RuntimeException]]
    * is thrown during the conversion, it is returned as an [[InvalidValue]] error.
    *
    * @param fn converts the input type to the output type, may throw [[RuntimeException]]s
    * @tparam In the input type of the decoder
    * @tparam Out the output type of the decoder
    * @return a new decoder that decodes [[In]] to [[Out]]
    */
  def unsafe[In <: JAny, Out](fn: In => Out): Decoder[In, Out] =
    Decoder { from =>
      try {
        fn(from).validNec
      } catch {
        case ex: RuntimeException => InvalidValue(from, ex.getMessage).invalidNec
      }
    }

  implicit def uuidDecoder[Context]: Decoder[JString, UUID] = unsafe(s => UUID.fromString(s.value))

  implicit val jnumberToByteDecoder: Decoder[JNumber, Byte] = unsafe(_.value.toByte)
  implicit val jnumberToShortDecoder: Decoder[JNumber, Short] = unsafe(_.value.toShort)
  implicit val jnumberToIntDecoder: Decoder[JNumber, Int] = unsafe(_.value.toInt)
  implicit val jnumberToLongDecoder: Decoder[JNumber, Long] = unsafe(_.value.toLong)
  implicit val jnumberToFloatDecoder: Decoder[JNumber, Float] = unsafe(_.value.toFloat)
  implicit val jnumberToDoubleDecoder: Decoder[JNumber, Double] = unsafe(_.value.toDouble)
  implicit val jnumberToBigIntDecoder: Decoder[JNumber, BigInt] = unsafe(n => BigInt(n.value))
  implicit val jnumberToBigDecDecoder: Decoder[JNumber, BigDecimal] = unsafe(n => BigDecimal.exact(n.value))

  implicit val localDateDecoder: Decoder[JString, LocalDate] = unsafe(s => LocalDate.parse(s.value))
  implicit val localTimeDecoder: Decoder[JString, LocalTime] = unsafe(s => LocalTime.parse(s.value))
  implicit val localDateTimeDecoder: Decoder[JString, LocalDateTime] = unsafe(s => LocalDateTime.parse(s.value))
  implicit val instantDecoder: Decoder[JString, Instant] = unsafe(s => Instant.parse(s.value))

  /** Creates a decoder that turns a JSON string into a numeric type.
    *
    * This behavior must be explicitly requested, otherwise it is ambiguous what to do when a [[JAny]] needs to be
    * decoded to a numeric type. There are two paths: one from a JSON number and one from a JSON string.
    *
    * @param decoder a decode from a JNumber to the output type
    * @tparam Out the output type of the new decoder
    * @tparam Context the context in which the decoder operates
    * @return a new decoder that decodes from a JSON string to a numeric type [[Out]]
    */

  def jstringToJNumber[Out, Context](implicit
      decoder: ContextualDecoder[JNumber, Out, Context]
  ): ContextualDecoder[JString, Out, Context] = { (s, context) =>
    // Perform the decoding by pretending like the JSON value was a string all along.
    decoder.decode(JNumber(s.value, s.location, s.pointer), context)
  }

  // Decoder is invariant in the input type so that we can better control it. These serve to narrow Decoders for any
  // of the JAny types to Decoders for JAny. The resulting Decoders will fail if the input JAny is not the expected
  // type.

  /** Creates a decoder that will attempt to narrow a [[JAny]] to a [[JNull]] where there is no specific [[JAny]]
    * decoder available. It will fail with a JsonTypeMismatch if it is not the correct JSON type. If it is the correct
    * JSON type, it will defer to the underlying decoder.
    */
  implicit def narrowNull[To, Context](implicit
      dec: ContextualDecoder[JNull, To, Context]
  ): ContextualDecoder[JAny, To, Context] = { (from, context) =>
    from.asNull.andThen(dec.decode(_, context))
  }

  /** Creates a decoder that will attempt to narrow a [[JAny]] to a [[JNumber]] where there is no specific [[JAny]]
    * decoder available. It will fail with a JsonTypeMismatch if it is not the correct JSON type. If it is the correct
    * JSON type, it will defer to the underlying decoder.
    */
  implicit def narrowNumber[To, Context](implicit
      dec: ContextualDecoder[JNumber, To, Context]
  ): ContextualDecoder[JAny, To, Context] = { (from, context) =>
    from.asNumber.andThen(dec.decode(_, context))
  }

  /** Creates a decoder that will attempt to narrow a [[JAny]] to a [[JString]] where there is no specific [[JAny]]
    * decoder available. It will fail with a JsonTypeMismatch if it is not the correct JSON type. If it is the correct
    * JSON type, it will defer to the underlying decoder.
    */
  implicit def narrowString[To, Context](implicit
      dec: ContextualDecoder[JString, To, Context]
  ): ContextualDecoder[JAny, To, Context] = { (from, context) =>
    from.asString.andThen(dec.decode(_, context))
  }

  /** Creates a decoder that will attempt to narrow a [[JAny]] to a [[JBoolean]] where there is no specific [[JAny]]
    * decoder available. It will fail with a JsonTypeMismatch if it is not the correct JSON type. If it is the correct
    * JSON type, it will defer to the underlying decoder.
    */
  implicit def narrowBoolean[To, Context](implicit
      dec: ContextualDecoder[JBoolean, To, Context]
  ): ContextualDecoder[JAny, To, Context] = { (from, context) =>
    from.asBoolean.andThen(dec.decode(_, context))
  }

  /** Creates a decoder that will attempt to narrow a [[JAny]] to a [[JObject]] where there is no specific [[JAny]]
    * decoder available. It will fail with a JsonTypeMismatch if it is not the correct JSON type. If it is the correct
    * JSON type, it will defer to the underlying decoder.
    */
  implicit def narrowObject[To, Context](implicit
      dec: ContextualDecoder[JObject, To, Context]
  ): ContextualDecoder[JAny, To, Context] = { (from, context) =>
    from.asObject.andThen(dec.decode(_, context))
  }

  /** Creates a decoder that will attempt to narrow a [[JAny]] to a [[JArray]] where there is no specific [[JAny]]
    * decoder available. It will fail with a JsonTypeMismatch if it is not the correct JSON type. If it is the correct
    * JSON type, it will defer to the underlying decoder.
    */
  implicit def narrowArray[To, Context](implicit
      dec: ContextualDecoder[JArray, To, Context]
  ): ContextualDecoder[JAny, To, Context] = { (from, context) =>
    from.asArray.andThen(dec.decode(_, context))
  }
}
