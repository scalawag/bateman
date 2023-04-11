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

package org.scalawag.bateman.json.enumeratum

import enumeratum.{Enum, EnumEntry}
import cats.syntax.either._
import cats.syntax.functor._
import cats.syntax.contravariant._
import org.scalawag.bateman.json.{Decoder, Encoder, InvalidValue, JError, JString, JStringDecoder, JStringEncoder}

trait BatemanEnum[A <: EnumEntry] { this: Enum[A] =>
  implicit val batemanEncoder: Encoder[A, JString] = BatemanEnum.encoder(this)
  implicit val batemanDecoder: Decoder[JString, A] = BatemanEnum.decoder(this)
}

object BatemanEnum {
  def encoder[A <: EnumEntry](e: Enum[A]): JStringEncoder[A] =
    JStringEncoder[String].contramap(_.entryName)

  def decoder[A <: EnumEntry](e: Enum[A]): JStringDecoder[A] =
    JStringDecoder { in =>
      e.withNameOption(in.value.value) match {
        case Some(member) => member.rightNec[JError]
        case None         => InvalidValue(in, s"'${in.value.value}' is not a member of enum $e").leftNec
      }
    }
}
