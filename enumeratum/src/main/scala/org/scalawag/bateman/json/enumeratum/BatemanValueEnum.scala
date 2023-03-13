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

package org.scalawag.bateman.json.enumeratum

import cats.syntax.validated._
import enumeratum.values._
import org.scalawag.bateman.json.{decoding, encoding}
import org.scalawag.bateman.json.decoding.{Decoder, InvalidValue, JNumberDecoder, JStringDecoder}
import org.scalawag.bateman.json.encoding.{Encoder, JNumberEncoder, JStringEncoder}

sealed trait BatemanValueEnum[
    ValueType,
    DecodingType <: decoding.JAny,
    EncodingType <: encoding.JAny,
    EntryType <: ValueEnumEntry[ValueType]
] {
  this: ValueEnum[ValueType, EntryType] =>
  implicit def batemanEncoder: Encoder[EntryType, EncodingType]
  implicit def batemanDecoder: Decoder[DecodingType, EntryType]
}

object BatemanValueEnum {
  def encoder[EncodingType <: encoding.JAny, ValueType, EntryType <: ValueEnumEntry[ValueType]](
      `enum`: ValueEnum[ValueType, EntryType]
  )(implicit valueEncoder: Encoder[ValueType, EncodingType]): Encoder[EntryType, EncodingType] = { entry =>
    valueEncoder.encode(entry.value)
  }

  def decoder[DecodingType <: decoding.JAny, ValueType, EntryType <: ValueEnumEntry[ValueType]](
      `enum`: ValueEnum[ValueType, EntryType]
  )(implicit valueDecoder: Decoder[DecodingType, ValueType]): Decoder[DecodingType, EntryType] =
    Decoder[DecodingType, EntryType] { in: DecodingType =>
      valueDecoder.decode(in).andThen { v =>
        `enum`.withValueOpt(v) match {
          case Some(member) => member.validNec
          case None         => InvalidValue(in, s"$v is not a member of enum ${`enum`}").invalidNec
        }
      }
    }
}

trait JNumberBatemanEnum[ValueType, EntryType <: ValueEnumEntry[ValueType]]
    extends BatemanValueEnum[ValueType, decoding.JNumber, encoding.JNumber, EntryType] {
  this: ValueEnum[ValueType, EntryType] =>
}

trait IntBatemanEnum[EntryType <: IntEnumEntry] extends JNumberBatemanEnum[Int, EntryType] {
  this: ValueEnum[Int, EntryType] =>
  implicit val batemanEncoder: JNumberEncoder[EntryType] = BatemanValueEnum.encoder(this)
  implicit val batemanDecoder: JNumberDecoder[EntryType] = BatemanValueEnum.decoder(this)
}

trait LongBatemanEnum[EntryType <: LongEnumEntry] extends JNumberBatemanEnum[Long, EntryType] {
  this: ValueEnum[Long, EntryType] =>
  implicit val batemanEncoder: JNumberEncoder[EntryType] = BatemanValueEnum.encoder(this)
  implicit val batemanDecoder: JNumberDecoder[EntryType] = BatemanValueEnum.decoder(this)
}

trait ShortBatemanEnum[EntryType <: ShortEnumEntry] extends JNumberBatemanEnum[Short, EntryType] {
  this: ValueEnum[Short, EntryType] =>
  implicit val batemanEncoder: JNumberEncoder[EntryType] = BatemanValueEnum.encoder(this)
  implicit val batemanDecoder: JNumberDecoder[EntryType] = BatemanValueEnum.decoder(this)
}

trait ByteBatemanEnum[EntryType <: ByteEnumEntry] extends JNumberBatemanEnum[Byte, EntryType] {
  this: ValueEnum[Byte, EntryType] =>
  implicit val batemanEncoder: JNumberEncoder[EntryType] = BatemanValueEnum.encoder(this)
  implicit val batemanDecoder: JNumberDecoder[EntryType] = BatemanValueEnum.decoder(this)
}

trait JStringBatemanEnum[ValueType, EntryType <: ValueEnumEntry[ValueType]]
    extends BatemanValueEnum[ValueType, decoding.JString, encoding.JString, EntryType] {
  this: ValueEnum[ValueType, EntryType] =>
}

trait StringBatemanEnum[EntryType <: StringEnumEntry] extends JStringBatemanEnum[String, EntryType] {
  this: ValueEnum[String, EntryType] =>
  implicit val batemanEncoder: JStringEncoder[EntryType] = BatemanValueEnum.encoder(this)
  implicit val batemanDecoder: JStringDecoder[EntryType] = BatemanValueEnum.decoder(this)
}

trait CharBatemanEnum[EntryType <: CharEnumEntry] extends JStringBatemanEnum[Char, EntryType] {
  this: ValueEnum[Char, EntryType] =>
  implicit val batemanEncoder: JStringEncoder[EntryType] = BatemanValueEnum.encoder(this)
  implicit val batemanDecoder: JStringDecoder[EntryType] = BatemanValueEnum.decoder(this)
}
