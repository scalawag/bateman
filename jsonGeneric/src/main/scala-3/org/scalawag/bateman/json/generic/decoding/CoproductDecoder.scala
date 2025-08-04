// bateman -- Copyright 2021-2026 -- Justin Patterson
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

package org.scalawag.bateman.json.generic.decoding

import cats.data.NonEmptyChain
import org.scalawag.bateman.json.*
import org.scalawag.bateman.json.focus.{JFocus, JFieldFocus}
import org.scalawag.bateman.json.generic.Config
import scala.compiletime.*
import scala.deriving.Mirror

object CoproductDecoder:
  inline def summonDecoders[T <: Tuple]: List[JObjectDecoder[?]] =
    inline erasedValue[T] match
      case _: EmptyTuple => Nil
      case _: (t *: ts) => summonInline[JObjectDecoder[t]] :: summonDecoders[ts]

  private def makeDecoder[T](decoders: List[JObjectDecoder[?]], labels: List[String], discriminatorField: String, config: Config): JObjectDecoder[T] =
    (jobjFocus, discriminatorFieldFocuses) =>
      val jobj = jobjFocus.value
      val fieldResult = jobj.fieldList.zipWithIndex
        .find(_._1.name.value == discriminatorField)
        .map((field, idx) => (field, JFieldFocus(field.value, field.name, idx, jobjFocus)))

      fieldResult match
        case Some((field, fieldFocus)) =>
          field.value match
            case JString(typeName, _) =>
              // Find the matching decoder based on the discriminator
              labels.zip(decoders).find { case (label, _) =>
                config.classNameMapping(label) == typeName
              } match
                case Some((_, decoder)) =>
                  decoder.asInstanceOf[JObjectDecoder[T]].decode(jobjFocus, discriminatorFieldFocuses)
                case None =>
                  val valids = labels.map(l => JString(config.classNameMapping(l)): JAny).toSet
                  Left(NonEmptyChain.one(InvalidDiscriminator(fieldFocus, valids)))
            case _ =>
              val valids = labels.map(l => JString(config.classNameMapping(l)): JAny).toSet
              Left(NonEmptyChain.one(InvalidDiscriminator(fieldFocus, valids)))

        case None =>
          Left(NonEmptyChain.one(MissingField(jobjFocus, discriminatorField)))

  inline def derived[T](discriminatorField: String = "type")(using m: Mirror.SumOf[T], config: Config): JObjectDecoder[T] =
    val decoders = summonDecoders[m.MirroredElemTypes]
    val labels = constValueTuple[m.MirroredElemLabels].toArray.map(_.toString).toList
    makeDecoder[T](decoders, labels, discriminatorField, config)
