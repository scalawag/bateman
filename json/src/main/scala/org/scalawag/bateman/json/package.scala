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

package org.scalawag.bateman

import cats.syntax.validated._
import cats.data.{NonEmptyChain, ValidatedNec}
import org.scalawag.bateman.json.decoding.JAny
import org.scalawag.bateman.json.decoding.parser.ParseResult

import scala.collection.compat.immutable.LazyList

package object json {

  /** Represents a codec that does not depend on the decoding context. */
  type Codec[Dec, A, Enc] = ContextualCodec[Dec, A, Enc, Any]

  type JAnyCodec[A] = Codec[decoding.JAny, A, encoding.JAny]
  type JArrayCodec[A] = Codec[decoding.JArray, A, encoding.JArray]
  type JObjectCodec[A] = Codec[decoding.JObject, A, encoding.JObject]
  type JStringCodec[A] = Codec[decoding.JString, A, encoding.JString]
  type JNumberCodec[A] = Codec[decoding.JNumber, A, encoding.JNumber]

  object JAnyCodec extends CodecAliasCompanion[decoding.JAny, encoding.JAny]
  object JArrayCodec extends CodecAliasCompanion[decoding.JArray, encoding.JArray]
  object JObjectCodec extends CodecAliasCompanion[decoding.JObject, encoding.JObject]
  object JStringCodec extends CodecAliasCompanion[decoding.JString, encoding.JString]
  object JNumberCodec extends CodecAliasCompanion[decoding.JNumber, encoding.JNumber]

  trait CodecAliasCompanion[Dec, Enc] {
    def apply[A](implicit codec: Codec[Dec, A, Enc]): Codec[Dec, A, Enc] = codec
  }

  type JAnyContextualCodec[A, Context] = ContextualCodec[decoding.JAny, A, encoding.JAny, Context]
  type JArrayContextualCodec[A, Context] = ContextualCodec[decoding.JArray, A, encoding.JArray, Context]
  type JObjectContextualCodec[A, Context] = ContextualCodec[decoding.JObject, A, encoding.JObject, Context]
  type JStringContextualCodec[A, Context] = ContextualCodec[decoding.JString, A, encoding.JString, Context]
  type JNumberContextualCodec[A, Context] = ContextualCodec[decoding.JNumber, A, encoding.JNumber, Context]

  object JAnyContextualCodec extends ContextualCodecAliasCompanion[decoding.JAny, encoding.JAny]
  object JArrayContextualCodec extends ContextualCodecAliasCompanion[decoding.JArray, encoding.JArray]
  object JObjectContextualCodec extends ContextualCodecAliasCompanion[decoding.JObject, encoding.JObject]
  object JStringContextualCodec extends ContextualCodecAliasCompanion[decoding.JString, encoding.JString]
  object JNumberContextualCodec extends ContextualCodecAliasCompanion[decoding.JNumber, encoding.JNumber]

  trait ContextualCodecAliasCompanion[Dec, Enc] {
    def apply[A, Context](implicit
        codec: ContextualCodec[Dec, A, Enc, Context]
    ): ContextualCodec[Dec, A, Enc, Context] = codec
  }

  /** Returns [[None]] if its argument is empty and a [[Some]] containing the argument if it is not.  */

  def noneIfEmpty[A <: Traversable[_]](a: A): Option[A] = if (a.isEmpty) None else Some(a)

  /** Returns an invalid result containing all the errors unless errors is empty, in which case, it returns a valid
    * result containing its second argument. */

  def validIfEmpty[E, A](errors: Iterable[E], b: => A): ValidatedNec[E, A] =
    NonEmptyChain.fromSeq(errors.toSeq).map(_.invalid).getOrElse(b.validNec)

  /** Parse an input string into a single [[decoding.JAny]].
    *
    * @param text the JSON text
    * @param source an optional name for the source of the text (e.g., the filename or URL) which will be included
    *               in error messages
    */
  def parse(text: LazyList[Char], source: Option[String]): ParseResult[JAny] =
    decoding.parser.toJAny(text, source)

  def parse(text: String, source: Option[String] = None): ParseResult[JAny] =
    decoding.parser.toJAny(text.to(LazyList), source)
}
