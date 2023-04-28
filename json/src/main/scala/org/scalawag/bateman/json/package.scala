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

package org.scalawag.bateman

import scala.language.higherKinds
import cats.syntax.either._
import cats.syntax.parallel._
import cats.data.{EitherNec, NonEmptyChain}
import org.scalawag.bateman.json.focus.{JFoci, JFocus, JRootFocus}

import scala.collection.compat.immutable.LazyList
import org.scalawag.bateman.json.parser.ParseResult

package object json {
  type JResult[+A] = EitherNec[JError, A]

  implicit class RichJResult[A](me: JResult[A]) {
    def getOrThrow: A = me.fold(ee => throw JErrors(ee), identity)
  }

  implicit class JResultFOps[F[+_], A](me: JResult[F[A]]) {

    /** Makes it easier to ignore MissingValues in results. */
    def ignoreMissingValuesWith(empty: F[A]): JResult[F[A]] =
      me match {
        case Right(b)                                            => b.rightNec
        case Left(ee) if ee.forall(_.isInstanceOf[MissingValue]) => empty.rightNec
        case Left(ee)                                            => ee.asLeft
      }
  }

  type JAnyDecoder[To] = Decoder[JAny, To]
  type JArrayDecoder[To] = Decoder[JArray, To]
  type JStringDecoder[To] = Decoder[JString, To]
  type JNumberDecoder[To] = Decoder[JNumber, To]
  type JBooleanDecoder[To] = Decoder[JBoolean, To]

  object JAnyDecoder extends DecoderAliasCompanion[JAny]
  object JArrayDecoder extends DecoderAliasCompanion[JArray]
  object JStringDecoder extends DecoderAliasCompanion[JString]
  object JNumberDecoder extends DecoderAliasCompanion[JNumber]
  object JBooleanDecoder extends DecoderAliasCompanion[JBoolean]

  trait DecoderAliasCompanion[From <: JAny] {

    /** Summons a specified decoder (which must already exist in implicit scope) by type. */
    def apply[To](implicit dec: Decoder[From, To]): Decoder[From, To] = dec

  }

  type JAnyEncoder[In] = Encoder[In, JAny]
  type JArrayEncoder[In] = Encoder[In, JArray]
  type JStringEncoder[In] = Encoder[In, JString]
  type JNumberEncoder[In] = Encoder[In, JNumber]
  type JBooleanEncoder[In] = Encoder[In, JBoolean]
  type JNullEncoder[In] = Encoder[In, JNull]

  object JAnyEncoder extends EncoderAliasCompanion[JAny, JAnyEncoder]
  object JArrayEncoder extends EncoderAliasCompanion[JArray, JArrayEncoder]
  object JStringEncoder extends EncoderAliasCompanion[JString, JStringEncoder]
  object JNumberEncoder extends EncoderAliasCompanion[JNumber, JNumberEncoder]
  object JBooleanEncoder extends EncoderAliasCompanion[JBoolean, JBooleanEncoder]
  object JNullEncoder extends EncoderAliasCompanion[JBoolean, JNullEncoder]

  trait EncoderAliasCompanion[Out, Enc[_]] {
    def apply[In](implicit encoder: Enc[In]): Enc[In] = encoder

    def encode[In](in: In)(implicit enc: Encoder[In, Out]): Out = enc.encode(in)
  }

  type JAnyCodec[A] = Codec[JAny, A]
  type JArrayCodec[A] = Codec[JArray, A]
  type JStringCodec[A] = Codec[JString, A]
  type JNumberCodec[A] = Codec[JNumber, A]
  type JBooleanCodec[A] = Codec[JBoolean, A]

  object JAnyCodec extends CodecAliasCompanion[JAny]
  object JArrayCodec extends CodecAliasCompanion[JArray]
  object JStringCodec extends CodecAliasCompanion[JString]
  object JNumberCodec extends CodecAliasCompanion[JNumber]
  object JBooleanCodec extends CodecAliasCompanion[JBoolean]

  trait CodecAliasCompanion[A <: JAny] {
    def apply[B](implicit codec: Codec[A, B]): Codec[A, B] = codec
    def apply[B](decoder: Decoder[A, B], encoder: Encoder[B, A]): Codec[A, B] = new Codec(encoder, decoder)
  }

  /** Returns [[scala.None None]] if its argument is empty and a [[scala.Some Some]] containing the argument if it is not. */

  def noneIfEmpty[A <: Traversable[_]](a: A): Option[A] = if (a.isEmpty) None else Some(a)

  /** Returns an invalid result containing all the errors unless errors is empty, in which case, it returns a valid
    * result containing its second argument. */

  def rightIfEmpty[E, A](errors: Iterable[E], b: => A): EitherNec[E, A] =
    NonEmptyChain.fromSeq(errors.toSeq).map(_.asLeft).getOrElse(b.rightNec)

  /** Parse an input string into a single [[JAny]].
    *
    * @param text   the JSON text
    * @param source an optional name for the source of the text (e.g., the filename or URL) which will be included
    *               in error messages
    */
  def parse(text: LazyList[Char], source: Option[String]): ParseResult[JRootFocus[JAny]] =
    parser.toJAny(text, source).map(jany => jany.asRootFocus)

  def parse(text: String, source: Option[String] = None): ParseResult[JRootFocus[JAny]] =
    parse(text.to(LazyList), source)

  def parseUnsafe(text: String, source: Option[String] = None): JFocus[JAny] =
    parse(text, source).fold(throw _, identity)

  // For convenience. Should these actually exist, though? TODO
  implicit class JResultIdOps[A <: JAny](me: JResult[JFocus[A]]) {
    import org.scalawag.bateman.json.focus.weak._
    def decode[B](implicit decoder: Decoder[A, B]): JResult[B] = me.flatMap(_.decode[B])
    val value: JResult[A] = me.map(_.value)
  }

  implicit class JResultOptionOps[A <: JAny](me: JResult[Option[JFocus[A]]]) {
    import org.scalawag.bateman.json.focus.weak._
    def decode[B](implicit decoder: Decoder[A, B]): JResult[Option[B]] = me.flatMap(_.parTraverse(_.decode[B]))
    val value: JResult[Option[A]] = me.map(_.map(_.value))
  }

  implicit class RichJResultList[A <: JAny](me: JResult[JFoci[A]]) {
    import org.scalawag.bateman.json.focus.weak._
    def decode[B](implicit decoder: Decoder[A, B]): JResult[List[B]] = me.flatMap(_.foci.parTraverse(_.decode[B]))
    val values: JResult[List[JAny]] = me.map(_.foci.map(_.value))
  }
}
