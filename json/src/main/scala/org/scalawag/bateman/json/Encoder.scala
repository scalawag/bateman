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

import cats.{Contravariant, Functor}
import org.scalawag.bateman.json.syntax._
import cats.syntax.contravariant._
import org.scalawag.bateman.json.focus.JFocus

import java.time.{Instant, LocalDate, LocalDateTime, LocalTime}
import java.util.UUID

trait Encoder[A, +B] {
  def encode(in: A): B
}

object Encoder {
  def apply[A, B](implicit encoder: Encoder[A, B]): Encoder[A, B] = encoder

  def encode[A, B](a: A)(implicit encoder: Encoder[A, B]): B = encoder.encode(a)

  implicit def identityEncoder[A]: Encoder[A, A] = identity(_)

  implicit def nullEncoder[A <: Null]: JNullEncoder[A] = _ => JNull
  implicit val booleanEncoder: JBooleanEncoder[Boolean] = JBoolean(_)

  implicit val charEncoder: JStringEncoder[Char] = c => JString(c.toString)
  implicit val stringEncoder: JStringEncoder[String] = JString(_)

  implicit val byteEncoder: JNumberEncoder[Byte] = JNumber(_)
  implicit val shortEncoder: JNumberEncoder[Short] = JNumber(_)
  implicit val intEncoder: JNumberEncoder[Int] = JNumber(_)
  implicit val longEncoder: JNumberEncoder[Long] = JNumber(_)
  implicit val floatEncoder: JNumberEncoder[Float] = JNumber(_)
  implicit val doubleEncoder: JNumberEncoder[Double] = JNumber(_)
  implicit val bigIntEncoder: JNumberEncoder[BigInt] = JNumber(_)
  implicit val bigDecEncoder: JNumberEncoder[BigDecimal] = JNumber(_)

  implicit val uuidEncoder: JStringEncoder[UUID] = stringEncoder.contramap(_.toString)

  implicit val localDateEncoder: JStringEncoder[LocalDate] = stringEncoder.contramap(_.toString)
  implicit val localTimeEncoder: JStringEncoder[LocalTime] = stringEncoder.contramap(_.toString)
  implicit val localDateTimeEncoder: JStringEncoder[LocalDateTime] = stringEncoder.contramap(_.toString)
  implicit val instantEncoder: JStringEncoder[Instant] = stringEncoder.contramap(_.toString)

  // Encodes the value ignoring the focus.
  implicit def focusEncoder[A <: JAny, B](implicit enc: Encoder[A, B]): Encoder[JFocus[A], B] = l => enc.encode(l.value)

  implicit def nullableEncoder[A: JAnyEncoder]: JAnyEncoder[Nullable[A]] = {
    case NotNull(a) => a.toJAny
    case Null       => JNull
  }

  implicit def seqEncoder[A: JAnyEncoder]: JArrayEncoder[Seq[A]] = { aa =>
    JArray(aa.map(_.toJAny): _*)
  }

  implicit def listEncoder[A: JAnyEncoder]: JArrayEncoder[List[A]] = seqEncoder[A].contramap(_.toSeq)

  implicit def mapEncoder[A: JStringEncoder, B: JAnyEncoder]: JObjectEncoder[Map[A, B]] = { (m, _) =>
    JObject(
      m.map { case (k, v) => JField(k.to[JString], v.toJAny) }.toList.sortBy(_.name.value),
      None
    )
  }

  implicit def pairsEncoder[A: JStringEncoder, B: JAnyEncoder]: JObjectEncoder[Seq[(A, B)]] = { (m, _) =>
    JObject(
      m.map { case (k, v) => JField(k.to[JString], v.toJAny) }.toList,
      None
    )
  }

  implicit def contravariantForEncoder[R]: Contravariant[Encoder[*, R]] =
    new Contravariant[Encoder[*, R]] {
      override def contramap[A, B](fa: Encoder[A, R])(f: B => A): Encoder[B, R] = b => fa.encode(f(b))
    }

  implicit def functorForEncoder[A]: Functor[Encoder[A, +*]] =
    new Functor[Encoder[A, +*]] {
      override def map[B, C](fa: Encoder[A, B])(f: B => C): Encoder[A, C] = b => f(fa.encode(b))
    }
}
