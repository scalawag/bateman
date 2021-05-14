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

import enumeratum.{Enum, EnumEntry}
import cats.syntax.validated._
import org.scalawag.bateman.json.decoding.{Decoder, InvalidValue}
import org.scalawag.bateman.json.encoding.Encoder
import org.scalawag.bateman.json.{decoding, encoding}

trait BatemanEnum[A <: EnumEntry] { this: Enum[A] =>
  implicit val batemanEncoder: Encoder[A, encoding.JString] = BatemanEnum.encoder(this)
  implicit val batemanDecoder: Decoder[decoding.JString, A] = BatemanEnum.decoder(this)
}

object BatemanEnum {
  def encoder[A <: EnumEntry](enum: Enum[A]): Encoder[A, encoding.JString] =
    Encoder[String, encoding.JString].contramap(_.entryName)

  def decoder[A <: EnumEntry](enum: Enum[A]): Decoder[decoding.JString, A] =
    Decoder { in =>
      enum.withNameOption(in.value) match {
        case Some(member) => member.validNec
        case None         => InvalidValue(in, s"'${in.value}' is not a member of enum $enum").invalidNec
      }
    }
}
