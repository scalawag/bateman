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

object syntax {
  implicit class RichBateman[A](a: A) {
    def as[B](implicit dec: decoding.Decoder[A, B]): decoding.DecodeResult[B] = dec.decode(a)
    def to[B](implicit enc: encoding.Encoder[A, B]): B = enc.encode(a)
    def toJAny(implicit enc: encoding.Encoder[A, encoding.JAny]): encoding.JAny = a.to[encoding.JAny]
  }
}
