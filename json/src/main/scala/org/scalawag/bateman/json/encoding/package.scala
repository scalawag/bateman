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

/** This package contains the JSON ADT for JSON texts that originate programmatically within the system and are
  * (potentially) rendered to strings. It contains no facilities for navigation or extraction of data. There is a
  * mirror ADT in [[decoding]] which does contain those facilities.
  */

package object encoding {
  type JAnyEncoder[In] = Encoder[In, JAny]
  type JArrayEncoder[In] = Encoder[In, JArray]
  type JObjectEncoder[In] = Encoder[In, JObject]
  type JStringEncoder[In] = Encoder[In, JString]
  type JNumberEncoder[In] = Encoder[In, JNumber]
  type JBooleanEncoder[In] = Encoder[In, JBoolean]

  object JAnyEncoder extends EncoderAliasCompanion[JAny, JAnyEncoder]
  object JArrayEncoder extends EncoderAliasCompanion[JArray, JArrayEncoder]
  object JObjectEncoder extends EncoderAliasCompanion[JObject, JObjectEncoder]
  object JStringEncoder extends EncoderAliasCompanion[JString, JStringEncoder]
  object JNumberEncoder extends EncoderAliasCompanion[JNumber, JNumberEncoder]
  object JBooleanEncoder extends EncoderAliasCompanion[JBoolean, JBooleanEncoder]

  trait EncoderAliasCompanion[Out, Enc[_]] {
    def apply[In](implicit encoder: Enc[In]): Enc[In] = encoder
    def encode[In](in: In)(implicit enc: Encoder[In, Out]): Out = enc.encode(in)
  }
}
