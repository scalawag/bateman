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

/*
package org.scalawag.bateman.json.generic

import cats.syntax.traverse._
import cats.syntax.validated._
import org.scalawag.bateman.json.JObject
import org.scalawag.bateman.json.generic.decoding.JSource

import scala.reflect.macros.whitebox.Context

/** This contains macros that validate the input classes prior to generating the codecs using shapeless.
 * This puts most of the logic into non-macro code (which was a goal). The macros generate the creation of
 * the factory that is used to create the actual codec. That's because if the macro call is made from library code,
 * the developer doesn't get an error message that reflects where the problem is in _their_ code.
 */

class Macros(override protected val c: Context) extends MacroBase(c) {
  import c.universe._

  protected override val supportedTags: Iterable[ClassSymbol] = Iterable(sourceTag)

  private def checkForDiscriminatorCollisions(
      discriminatorField: String,
      subclasses: Set[ClassSymbol]
  ): MacroResult[Unit] =
    subclasses.toList
      .filterNot(_.isAbstract)
      .traverse { sc =>
        asCaseClass(sc.typeSignature).andThen { cc =>
          if (cc.fields.exists(_.name == discriminatorField))
            s"the subclass ${cc.sym} has a field '$discriminatorField' which collides with the configured discriminator".leftNec
          else
            ().rightNec
        }
      }
      .map(_ => ())

  // These just search for the Encoders that we know we're going to need so that the errors for missing implicits are
  // more useful than the normal all-or-nothing "implicit not found" message. Also speeds up resolution.
  private def encoderPreChecks(fields: List[Field]): List[Tree] = {
    // Only generate one implicit for each type, not for each field. Fields can have the same type and then we'll get
    // ambiguous implicits if we have one for each field.
    val requiredInstances = fields.filterNot(_.hasTag(sourceTag)).groupByEq(f => stripOption(f.typ)).toList

    requiredInstances.map {
      case (t, ff) =>
        val name = instanceName("encoder", ff)
        maybeNamedImplicit(
          name,
          q"""implicitly[_root_.shapeless.Lazy[org.scalawag.bateman.json.JAnyEncoder[$t]]]"""
        )
    }
  }

  private def encoderPreChecks(subclasses: Set[ClassSymbol]): List[Tree] =
    subclasses.toList.map { cs =>
      val name = instanceName("encoder", cs)
      maybeNamedImplicit(
        name,
        q"""implicitly[org.scalawag.bateman.json.generic.encoding.CaseClassEncoder[$cs]]"""
      )
    }

  // These just search for the Decoders that we know we're going to need so that the errors for missing implicits are
  // more useful than the normal all-or-nothing "implicit not found" message. Also speeds up resolution.
  private def decoderPreChecks[To: WeakTypeTag](fields: List[Field]): List[Tree] = {
    val requiredInstances = fields.filterNot(_.hasTag(sourceTag)).groupByEq(f => stripOption(f.typ)).toList

    requiredInstances.map {
      case (t, ff) =>
        val name = instanceName("decoder", ff)
        val to = weakTypeOf[To]
        maybeNamedImplicit(
          name,
          q"""implicitly[_root_.shapeless.Lazy[org.scalawag.bateman.json.Decoder[org.scalawag.bateman.json.JAny, $t, $to]]]"""
        )
    }

  }

  private def decoderPreChecks[To: WeakTypeTag](subclasses: Set[ClassSymbol]): List[Tree] =
    subclasses.toList.map { cs =>
      val name = instanceName("decoder", cs)
      val to = weakTypeOf[To]
      maybeNamedImplicit(
        name,
        q"""implicitly[org.scalawag.bateman.json.generic.JObjectDecoder[$cs, $to]]"""
      )
    }

  private def caseClassCode[To: WeakTypeTag](
      fields: List[Field],
      decode: Boolean,
      encode: Boolean,
      deriver: Tree
  ): Tree =
    q"""{
      ..${if (decode) decoderPreChecks[To](fields) else Nil}
      ..${if (encode) encoderPreChecks(fields) else Nil}
      $deriver
    }"""

  private def traitCode[To: WeakTypeTag](
      subclasses: Set[ClassSymbol],
      decode: Boolean,
      encode: Boolean,
      deriver: Tree
  ): Tree =
    q"""{
      ..${if (decode) decoderPreChecks[To](subclasses) else Nil}
      ..${if (encode) encoderPreChecks(subclasses) else Nil}
      $deriver
    }"""

  //====================================================================================================================
  // The public methods that correspond to the ones in semiauto...
  //====================================================================================================================

  def deriveEncoderForTrait[A: WeakTypeTag]: Tree =
    getOrAbort[A] { a =>
      asSealedTrait(a).andThen {
        case SealedTrait(_, subclasses) =>
          checkForDiscriminatorCollisions(Config.get.discriminatorField, subclasses).map { _ =>
            traitCode[Nothing](
              subclasses,
              decode = false,
              encode = true,
              q"_root_.org.scalawag.bateman.json.generic.semiauto.unchecked.deriveEncoderForTrait[$a]"
            )
          }
      }
    }

  def deriveEncoderForCaseClass[A: WeakTypeTag]: Tree =
    getOrAbort[A] { a =>
      asCaseClass(a).andThen {
        case CaseClass(_, fields) =>
          caseClassCode[Nothing](
            fields,
            decode = false,
            encode = true,
            q"_root_.org.scalawag.bateman.json.generic.semiauto.unchecked.deriveEncoderForCaseClass[$a]"
          ).rightNec
      }
    }

  def deriveDecoderForTrait[A: WeakTypeTag, C: WeakTypeTag]: Tree =
    getOrAbort[A] { a =>
      asSealedTrait(a).andThen {
        case SealedTrait(_, subclasses) =>
          checkForDiscriminatorCollisions(Config.get.discriminatorField, subclasses).map { _ =>
            traitCode[C](
              subclasses,
              decode = true,
              encode = false,
              q"_root_.org.scalawag.bateman.json.generic.semiauto.unchecked.deriveDecoderForTrait[$a, ${weakTypeOf[C]}]"
            )
          }
      }
    }

  def deriveDecoderForCaseClass[A: WeakTypeTag, C: WeakTypeTag]: Tree =
    getOrAbort[A] { a =>
      asCaseClass(a).andThen {
        case CaseClass(_, fields) =>
          validateSourceField[JSource](fields).map { _ =>
            caseClassCode[C](
              fields,
              decode = true,
              encode = false,
              q"_root_.org.scalawag.bateman.json.generic.semiauto.unchecked.deriveDecoderForCaseClass[$a, ${weakTypeOf[C]}]"
            )
          }
      }
    }

  def deriveCodecForTrait[A: WeakTypeTag, C: WeakTypeTag]: Tree =
    getOrAbort[A] { a =>
      asSealedTrait(a).andThen {
        case SealedTrait(_, subclasses) =>
          checkForDiscriminatorCollisions(Config.get.discriminatorField, subclasses).map { _ =>
            traitCode[C](
              subclasses,
              decode = true,
              encode = true,
              q"_root_.org.scalawag.bateman.json.generic.semiauto.unchecked.deriveCodecForTrait[$a, ${weakTypeOf[C]}]"
            )
          }
      }
    }

  def deriveCodecForCaseClass[A: WeakTypeTag, C: WeakTypeTag]: Tree =
    getOrAbort[A] { a =>
      asCaseClass(a).andThen {
        case CaseClass(_, fields) =>
          validateSourceField[JSource](fields).map { _ =>
            caseClassCode[C](
              fields,
              decode = true,
              encode = true,
              q"_root_.org.scalawag.bateman.json.generic.semiauto.unchecked.deriveCodecForCaseClass[$a, ${weakTypeOf[C]}]"
            )
          }
      }
    }
}
 */
