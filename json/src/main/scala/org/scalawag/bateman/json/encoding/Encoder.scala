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

import cats.{Contravariant, Functor}
import org.scalawag.bateman.json.{NotNull, Null, Nullable}
import cats.syntax.contravariant._

import java.time.{Instant, LocalDate, LocalDateTime, LocalTime}
import java.util.UUID

trait Encoder[-A, +B] {
  def encode(a: A): B
}

object Encoder {
  implicit def contravariantForEncoder[R]: Contravariant[Encoder[-*, R]] =
    new Contravariant[Encoder[-*, R]] {
      override def contramap[A, B](fa: Encoder[A, R])(f: B => A): Encoder[B, R] = b => fa.encode(f(b))
    }

  implicit def functorForEncoder[A]: Functor[Encoder[A, +*]] =
    new Functor[Encoder[A, +*]] {
      override def map[B, C](fa: Encoder[A, B])(f: B => C): Encoder[A, C] = b => f(fa.encode(b))
    }

  def apply[A, B](implicit encoder: Encoder[A, B]): Encoder[A, B] = encoder

  def encode[A, B](a: A)(implicit encoder: Encoder[A, B]): B = encoder.encode(a)

  implicit def identityEncoder[A]: Encoder[A, A] = identity(_)

  implicit val charEncoder: JStringEncoder[Char] = c => JString(c.toString)
  implicit val stringEncoder: JStringEncoder[String] = JString(_)

  implicit val booleanEncoder: JBooleanEncoder[Boolean] = JBoolean(_)

  implicit val byteEncoder: JNumberEncoder[Byte] = JNumber(_)
  implicit val shortEncoder: JNumberEncoder[Short] = JNumber(_)
  implicit val intEncoder: JNumberEncoder[Int] = JNumber(_)
  implicit val longEncoder: JNumberEncoder[Long] = JNumber(_)
  implicit val floatEncoder: JNumberEncoder[Float] = JNumber(_)
  implicit val doubleEncoder: JNumberEncoder[Double] = JNumber(_)
  implicit val bigIntEncoder: JNumberEncoder[BigInt] = JNumber(_)
  implicit val bigDecEncoder: JNumberEncoder[BigDecimal] = JNumber(_)

  implicit val uuidEncoder: JStringEncoder[UUID] = stringEncoder.contramap(_.toString)

  implicit val localDateDecoder: JStringEncoder[LocalDate] = stringEncoder.contramap(_.toString)
  implicit val localTimeDecoder: JStringEncoder[LocalTime] = stringEncoder.contramap(_.toString)
  implicit val localDateTimeDecoder: JStringEncoder[LocalDateTime] = stringEncoder.contramap(_.toString)
  implicit val instantDecoder: JStringEncoder[Instant] = stringEncoder.contramap(_.toString)

  implicit def nullableEncoder[A](implicit enc: JAnyEncoder[A]): JAnyEncoder[Nullable[A]] = {
    case NotNull(a) => enc.encode(a)
    case _: Null    => JNull
  }

  implicit def listEncoder[A](implicit enc: JAnyEncoder[A]): JArrayEncoder[List[A]] = { aa =>
    JArray(aa.map(enc.encode): _*)
  }

  implicit def mapEncoder[A](implicit enc: JAnyEncoder[A]): JObjectEncoder[Map[String, A]] = { aa =>
    JObject(aa.toSeq.map {
      case (n, a) => n -> enc.encode(a)
    }: _*)
  }
}
