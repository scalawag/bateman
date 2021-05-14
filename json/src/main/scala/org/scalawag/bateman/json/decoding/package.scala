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

package org.scalawag.bateman.json

import cats.syntax.validated._
import cats.data.{NonEmptyChain, ValidatedNec}

/** This package contains the JSON ADT for JSON texts that begin as strings and are parsed and decoded within the
  * system. There is a mirror ADT in [[encoding]] that is for JSON texts that originate in code and are (potentially)
  * rendered to strings.
  */

package object decoding {
  type DecodeResult[+A] = ValidatedNec[DecodeError, A]

  object DecodeResult {

    /** Creates a valid result from the input argument. */
    def pure[A](a: A): DecodeResult[A] = a.validNec
  }

  /** Represents a decoder that does not depend on its context. */
  type Decoder[From, To] = ContextualDecoder[From, To, Any]

  object Decoder {

    /** Summons a specifed decoder (which must already exist in implicit scope) by type. */
    def apply[From, To](implicit dec: Decoder[From, To]): Decoder[From, To] = dec

    /** Creates a decoder (that does not depend on its context) from a function. */
    def apply[From, To](fn: From => DecodeResult[To]): Decoder[From, To] = { (from, _) => fn(from) }

    /** Creates a decoder (that does not depend on its context) from a partial function. */
    def fromPF[From, To](fn: PartialFunction[From, DecodeResult[To]]): Decoder[From, To] = { (from, _) => fn(from) }
  }

  type JAnyDecoder[To] = ContextualDecoder[JAny, To, Any]
  type JArrayDecoder[To] = ContextualDecoder[JArray, To, Any]
  type JObjectDecoder[To] = ContextualDecoder[JObject, To, Any]
  type JStringDecoder[To] = ContextualDecoder[JString, To, Any]
  type JNumberDecoder[To] = ContextualDecoder[JNumber, To, Any]

  object JAnyDecoder extends DecoderAliasCompanion[JAny]
  object JArrayDecoder extends DecoderAliasCompanion[JArray]
  object JObjectDecoder extends DecoderAliasCompanion[JObject]
  object JStringDecoder extends DecoderAliasCompanion[JString]
  object JNumberDecoder extends DecoderAliasCompanion[JNumber]

  trait DecoderAliasCompanion[From] {

    /** Summons a specifed decoder (which must already exist in implicit scope) by type. */
    def apply[To](implicit dec: ContextualDecoder[From, To, Any]): ContextualDecoder[From, To, Any] = dec

    /** Creates a decoder (that does not depend on its context) from a function. */
    def apply[To](fn: From => DecodeResult[To]): Decoder[From, To] = { (from, _) => fn(from) }

    /** Creates a decoder (that does not depend on its context) from a partial function. */
    def fromPF[To](fn: PartialFunction[From, DecodeResult[To]]): Decoder[From, To] = { (from, _) => fn(from) }
  }

  type JAnyContextualDecoder[To, Context] = ContextualDecoder[JAny, To, Context]
  type JArrayContextualDecoder[To, Context] = ContextualDecoder[JArray, To, Context]
  type JObjectContextualDecoder[To, Context] = ContextualDecoder[JObject, To, Context]
  type JStringContextualDecoder[To, Context] = ContextualDecoder[JString, To, Context]
  type JNumberContextualDecoder[To, Context] = ContextualDecoder[JNumber, To, Context]

  object JAnyContextualDecoder extends ContextualDecoderAliasCompanion[JAny]
  object JArrayContextualDecoder extends ContextualDecoderAliasCompanion[JArray]
  object JObjectContextualDecoder extends ContextualDecoderAliasCompanion[JObject]
  object JStringContextualDecoder extends ContextualDecoderAliasCompanion[JString]
  object JNumberContextualDecoder extends ContextualDecoderAliasCompanion[JNumber]

  trait ContextualDecoderAliasCompanion[From] {

    /** Summons a specifed decoder (which must already exist in implicit scope) by type. */
    def apply[To, Context](implicit d: ContextualDecoder[From, To, Context]): ContextualDecoder[From, To, Context] = d
  }
}
