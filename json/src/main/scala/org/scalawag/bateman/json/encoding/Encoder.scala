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

package org.scalawag.bateman.json.encoding

import org.scalawag.bateman.json.{NotNull, Null, Nullable}

import java.time.{Instant, LocalDate, LocalDateTime, LocalTime}
import java.util.UUID

trait Encoder[A, +B] {
  def encode(a: A): B

  def contramap[C](fn: C => A): Encoder[C, B] = { c =>
    encode(fn(c))
  }
}

object Encoder {
  def apply[A, B](implicit encoder: Encoder[A, B]): Encoder[A, B] = encoder

  def encode[A, B](a: A)(implicit encoder: Encoder[A, B]): B = encoder.encode(a)

  implicit def identityEncoder[A]: Encoder[A, A] = identity(_)

  implicit val charEncoder: Encoder[Char, JString] = c => JString(c.toString)
  implicit val stringEncoder: Encoder[String, JString] = JString(_)

  implicit val booleanEncoder: Encoder[Boolean, JBoolean] = JBoolean(_)

  implicit val byteEncoder: Encoder[Byte, JNumber] = JNumber(_)
  implicit val shortEncoder: Encoder[Short, JNumber] = JNumber(_)
  implicit val intEncoder: Encoder[Int, JNumber] = JNumber(_)
  implicit val longEncoder: Encoder[Long, JNumber] = JNumber(_)
  implicit val floatEncoder: Encoder[Float, JNumber] = JNumber(_)
  implicit val doubleEncoder: Encoder[Double, JNumber] = JNumber(_)
  implicit val bigIntEncoder: Encoder[BigInt, JNumber] = JNumber(_)
  implicit val bigDecEncoder: Encoder[BigDecimal, JNumber] = JNumber(_)

  implicit val uuidEncoder: Encoder[UUID, JString] = Encoder[String, JString].contramap(_.toString)

  implicit val localDateDecoder: Encoder[LocalDate, JString] = stringEncoder.contramap(_.toString)
  implicit val localTimeDecoder: Encoder[LocalTime, JString] = stringEncoder.contramap(_.toString)
  implicit val localDateTimeDecoder: Encoder[LocalDateTime, JString] = stringEncoder.contramap(_.toString)
  implicit val instantDecoder: Encoder[Instant, JString] = stringEncoder.contramap(_.toString)

  implicit def nullableEncoder[A](implicit enc: Encoder[A, JAny]): Encoder[Nullable[A], JAny] = {
    case NotNull(a) => enc.encode(a)
    case _: Null    => JNull
  }

  implicit def listEncoder[A](implicit enc: Encoder[A, JAny]): Encoder[List[A], JArray] = { aa =>
    JArray(aa.map(enc.encode): _*)
  }

  implicit def mapEncoder[A](implicit enc: Encoder[A, JAny]): Encoder[Map[String, A], JObject] = { aa =>
    JObject(aa.toSeq.map {
      case (n, a) => n -> enc.encode(a)
    }: _*)
  }
}
