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

package org.scalawag.bateman.jsonapi.generic

import cats.Eq

import scala.reflect.macros.whitebox.Context
import cats.syntax.validated._
import cats.syntax.traverse._
import cats.syntax.apply._
import org.scalawag.bateman.json.generic.MacroBase
import org.scalawag.bateman.jsonapi.decoding.{ResourceIdentifier, ResourceObject, ResourceObjectOptionalId}
import org.scalawag.bateman.jsonapi
import org.scalawag.bateman.jsonapi.generic.decoding.JSource

import scala.annotation.tailrec
import scala.collection.mutable.Builder
import scala.collection.{TraversableLike, immutable, mutable}

/** This contains macros that validate the input classes prior to generating the codecs using shapeless.
  * This puts most of the logic into non-macro code (which was a goal). The macros generate the creation of
  * the factory that is used to create the actual codec. That's because if the macro call is made from library code,
  * the developer doesn't get an error message that reflects where the problem is in _their_ code.
  */

class Macros(override protected val c: Context) extends MacroBase(c) {
  import c.universe._

  private val listType = typeOf[List[_]].typeSymbol.asClass

  private val idTag = typeOf[IdTag].typeSymbol.asClass
  private val attributeTag = typeOf[AttributeTag].typeSymbol.asClass
  private val relationshipTag = typeOf[RelationshipTag].typeSymbol.asClass
  private val metaTag = typeOf[MetaTag].typeSymbol.asClass

  override protected val supportedTags: Iterable[c.universe.ClassSymbol] =
    Iterable(sourceTag, idTag, attributeTag, relationshipTag, metaTag)

  private def allFieldsTaggedOnce(fields: List[Field]): MacroResult[Unit] =
    fields
      .traverse { f =>
        if (f.tag.isEmpty) {
          if (f.name == "id")
            s"field '${f.name}' must be tagged with $idTag".invalidNec
          else
            s"field '${f.name}' must be tagged with ${attributeTag.name} or ${relationshipTag.name}".invalidNec
        } else if (f.tag.contains(idTag) && f.name != "id")
          s"field '${f.name}' has the IdTag but is not named 'id'".invalidNec
        else
          ().validNec
      }
      .map(_ => ())

  private def requireId(fields: List[Field]): MacroResult[Unit] =
    if (fields.exists(_.name == "id"))
      ().validNec
    else
      "field 'id' is missing".invalidNec

  private def disallowTypeField(fields: List[Field]): MacroResult[Unit] =
    if (fields.exists(_.name == "type"))
      "field named 'type' is not allowed".invalidNec
    else
      ().validNec

  private def disallowAttributesAndRelationships(fields: List[Field]): MacroResult[Unit] = {
    fields.traverse { f =>
      if (f.tag.contains(attributeTag))
        s"field '${f.name}' (with AttributeTag) is not allowed in a resource identifier".invalidNec
      else if (f.tag.contains(relationshipTag))
        s"field '${f.name}' (with RelationshipTag) is not allowed in a resource identifier".invalidNec
      else
        ().validNec
    }
  }.map(_ => ())

  private def validateCaseClassForResourceIdentifier[In: WeakTypeTag](cc: CaseClass): MacroResult[CaseClass] =
    (
      allFieldsTaggedOnce(cc.fields),
      requireId(cc.fields),
      disallowTypeField(cc.fields),
      disallowAttributesAndRelationships(cc.fields),
    ).tupled.map(_ => cc)

  private def validateCaseClassForResourceObject[In: WeakTypeTag](cc: CaseClass): MacroResult[CaseClass] =
    (
      allFieldsTaggedOnce(cc.fields),
      requireId(cc.fields),
      disallowTypeField(cc.fields),
    ).tupled.map(_ => cc)

  private def validateCaseClassForResourceObjectOptionalId[In: WeakTypeTag](cc: CaseClass): MacroResult[CaseClass] =
    (
      allFieldsTaggedOnce(cc.fields),
      disallowTypeField(cc.fields),
    ).tupled.map(_ => cc)

  private val resourceIdentifierTypes = Iterable(
    weakTypeOf[jsonapi.encoding.ResourceIdentifier],
    weakTypeOf[jsonapi.decoding.ResourceIdentifier]
  )

  private def resourceIdentifierType[R: WeakTypeTag]: Boolean =
    resourceIdentifierTypes.exists(_ =:= weakTypeOf[R])

  private val resourceObjectTypes =
    Iterable(
      weakTypeOf[jsonapi.encoding.ResourceObject],
      weakTypeOf[jsonapi.decoding.ResourceObject]
    )

  private def resourceObjectType[R: WeakTypeTag]: Boolean =
    resourceObjectTypes.exists(_ =:= weakTypeOf[R])

  private val resourceObjectOptionalIdTypes =
    Iterable(
      weakTypeOf[jsonapi.encoding.ResourceObjectOptionalId],
      weakTypeOf[jsonapi.decoding.ResourceObjectOptionalId]
    )

  private def resourceObjectOptionalIdType[R: WeakTypeTag]: Boolean =
    resourceObjectOptionalIdTypes.exists(_ =:= weakTypeOf[R])

  private def validateCaseClassForResourceType[R: WeakTypeTag](cc: CaseClass): MacroResult[Unit] =
    if (resourceIdentifierType[R])
      (
        validateCaseClassForResourceIdentifier(cc),
        validateSourceField[JSource](cc.fields)
      ).tupled.map(_ => ())
    else if (resourceObjectType[R])
      (
        validateCaseClassForResourceObject(cc),
        validateSourceField[JSource](cc.fields)
      ).tupled.map(_ => ())
    else if (resourceObjectOptionalIdType[R])
      (
        validateCaseClassForResourceObjectOptionalId(cc),
        validateSourceField[JSource](cc.fields)
      ).tupled.map(_ => ())
    else
      ???

  private def getContainedType(t: Type): Type =
    if (isAssignableTo(t.typeSymbol.asClass, optionType))
      t.typeArgs.head
    else if (isAssignableTo(t.typeSymbol.asClass, listType))
      t.typeArgs.head
    else
      t

  // Get the list of Fields for which we need to find typeclass instances. This excludes relationships to the type
  // we're generating the codec for and source fields. Also, group them by
  private def instanceFields[A: WeakTypeTag](ff: List[Field]): List[((ClassSymbol, Type), List[Field])] =
    ff.filterNot(_.hasTag(sourceTag))
      .filterNot(f => f.hasTag(relationshipTag) && getContainedType(f.typ) =:= weakTypeOf[A])
      .groupByEq { f =>
        val tag = f.tag.getOrElse {
          c.abort(
            c.enclosingPosition,
            s"There's a bug in the macro. All fields should have been validated by now. ${f.name}"
          )
        }
        (tag, f.typ)
      }
      .toList

  // These just search for the Encoders that we know we're going to need so that the errors for missing implicits are
  // more useful than the normal all-or-nothing "implicit not found" message.
  private def encoderPreChecks[In <: jsonapi.encoding.ResourceLike: WeakTypeTag](fields: List[Field]): List[Tree] =
    instanceFields[In](fields).map {
      case ((tag, t), ff) if tag == idTag =>
        val name = instanceName("encoder", ff)
        val ct = getContainedType(t)
        maybeNamedImplicit(name, q"""implicitly[org.scalawag.bateman.json.encoding.JStringEncoder[$ct]]""")
      case ((tag, t), ff) if tag == attributeTag || tag == metaTag =>
        val name = instanceName("encoder", ff)
        maybeNamedImplicit(name, q"""implicitly[org.scalawag.bateman.json.encoding.JAnyEncoder[$t]]""")
      case ((tag, t), ff) if tag == relationshipTag =>
        val name = instanceName("encoder", ff)
        val ct = getContainedType(t)
        maybeNamedImplicit(
          name,
          q"""implicitly[org.scalawag.bateman.jsonapi.encoding.ResourceEncoder[$ct, org.scalawag.bateman.jsonapi.encoding.ResourceIdentifierLike]]"""
        )
    }

  // These just search for the Decoders that we know we're going to need so that the errors for missing implicits are
  // more useful than the normal all-or-nothing "implicit not found" message.
  private def decoderPreChecks[Out <: jsonapi.decoding.ResourceLike: WeakTypeTag](fields: List[Field]): List[Tree] =
    instanceFields[Out](fields).map {
      case ((tag, t), ff) if tag == idTag =>
        val name = instanceName("decoder", ff)
        val ct = getContainedType(t)
        maybeNamedImplicit(
          name,
          q""" implicitly[org.scalawag.bateman.json.decoding.JStringContextualDecoder[$ct, org.scalawag.bateman.jsonapi.decoding.Document]]"""
        )
      case ((tag, t), ff) if tag == attributeTag || tag == metaTag =>
        val name = instanceName("decoder", ff)
        maybeNamedImplicit(
          name,
          q"""implicitly[org.scalawag.bateman.json.decoding.JAnyContextualDecoder[$t, org.scalawag.bateman.jsonapi.decoding.Document]]"""
        )
      case ((tag, t), ff) if tag == relationshipTag =>
        val name = instanceName("decoder", ff)
        val ct = getContainedType(t)
        maybeNamedImplicit(
          name,
          q"""implicitly[org.scalawag.bateman.json.decoding.ContextualDecoder[org.scalawag.bateman.jsonapi.decoding.ResourceIdentifierLike, $ct, org.scalawag.bateman.jsonapi.decoding.Document]]"""
        )
    }

  private def encoderPreChecks[Out <: jsonapi.encoding.ResourceLike: WeakTypeTag](
      subclasses: Set[ClassSymbol]
  ): List[Tree] =
    subclasses.toList.map { sc =>
      val name = instanceName("encoder", sc)
      val out = weakTypeOf[Out]
      q"""implicit val $name = implicitly[org.scalawag.bateman.jsonapi.encoding.ResourceEncoder[$sc, $out]]"""
    }

  private def decoderPreChecks[In <: jsonapi.decoding.ResourceLike: WeakTypeTag](
      subclasses: Set[ClassSymbol]
  ): List[Tree] =
    subclasses.toList.map { sc =>
      val name = instanceName("decoder", sc)
      val in = weakTypeOf[In]
      q"""implicit val $name = implicitly[org.scalawag.bateman.jsonapi.decoding.ResourceDecoder[$in, $sc]]"""
    }

  //====================================================================================================================
  // The public methods that correspond to the ones in semiauto...
  //====================================================================================================================

  def deriveResourceEncoderForTrait[A: WeakTypeTag, Out <: jsonapi.encoding.ResourceLike: WeakTypeTag]: Tree = {
    getOrAbort[A] { a =>
      asSealedTrait(a).map { sealedTrait =>
        val out = weakTypeOf[Out]
        q"""{
          ..${encoderPreChecks[Out](sealedTrait.subclasses)}
          _root_.org.scalawag.bateman.jsonapi.generic.semiauto.unchecked.deriveResourceEncoderForTrait[$a, $out]
        }"""
      }
    }
  }

  def deriveResourceEncoderForCaseClass[A: WeakTypeTag, Out <: jsonapi.encoding.ResourceLike: WeakTypeTag]: Tree =
    getOrAbort[A] { a =>
      asCaseClass(a).andThen { cc =>
        validateCaseClassForResourceType[Out](cc).map { _ =>
          val out = weakTypeOf[Out]
          q"""{
            ..${encoderPreChecks[Out](cc.fields)}
            _root_.org.scalawag.bateman.jsonapi.generic.semiauto.unchecked.deriveResourceEncoderForCaseClass[$a, $out]
          }"""
        }
      }
    }

  def deriveResourceDecoderForTrait[In <: jsonapi.decoding.ResourceLike: WeakTypeTag, A: WeakTypeTag]: Tree = {
    getOrAbort[A] { a =>
      asSealedTrait(a).map { sealedTrait =>
        val in = weakTypeOf[In]
        q"""{
          ..${decoderPreChecks[In](sealedTrait.subclasses)}
          _root_.org.scalawag.bateman.jsonapi.generic.semiauto.unchecked.deriveResourceDecoderForTrait[$in, $a]
        }"""
      }
    }
  }

  def deriveResourceDecoderForCaseClass[In <: jsonapi.decoding.ResourceLike: WeakTypeTag, A: WeakTypeTag]: Tree =
    getOrAbort[A] { a =>
      asCaseClass(a).andThen { cc =>
        validateCaseClassForResourceType[In](cc).map { _ =>
          val in = weakTypeOf[In]
          q"""{
            ..${decoderPreChecks[In](cc.fields)}
            _root_.org.scalawag.bateman.jsonapi.generic.semiauto.unchecked.deriveResourceDecoderForCaseClass[$in, $a]
          }"""
        }
      }
    }

  def deriveResourceCodecForTrait[
      In <: jsonapi.decoding.ResourceLike: WeakTypeTag,
      A: WeakTypeTag,
      Out <: jsonapi.encoding.ResourceLike: WeakTypeTag
  ]: Tree = {
    getOrAbort[A] { a =>
      asSealedTrait(a).map { sealedTrait =>
        val in = weakTypeOf[In]
        val out = weakTypeOf[Out]
        q"""{
          ..${encoderPreChecks[Out](sealedTrait.subclasses)}
          ..${decoderPreChecks[In](sealedTrait.subclasses)}
          _root_.org.scalawag.bateman.jsonapi.generic.semiauto.unchecked.deriveResourceCodecForTrait[$in, $a, $out]
        }"""
      }
    }
  }

  def deriveResourceCodecForCaseClass[
      In <: jsonapi.decoding.ResourceLike: WeakTypeTag,
      A: WeakTypeTag,
      Out <: jsonapi.encoding.ResourceLike: WeakTypeTag
  ]: Tree =
    getOrAbort[A] { a =>
      asCaseClass(a).andThen { cc =>
        validateCaseClassForResourceType[Out](cc).map { _ =>
          val in = weakTypeOf[In]
          val out = weakTypeOf[Out]
          q"""{
            ..${encoderPreChecks[Out](cc.fields)}
            ..${decoderPreChecks[In](cc.fields)}
            _root_.org.scalawag.bateman.jsonapi.generic.semiauto.unchecked.deriveResourceCodecForCaseClass[$in, $a, $out]
          }"""
        }
      }
    }
}
