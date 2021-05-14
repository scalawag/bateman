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

import scala.language.experimental.macros

import org.scalawag.bateman.jsonapi

/** Contains every combination of a three-dimensional space.
  *
  * CaseClass/Trait
  * Decoder/Encoder/Codec
  *
  * Functions are then parameterized by the resource type:
  * ResourceIdentifier/ResourceObject/ResourceObjectOptionalId
  *
  * Names are created by concatenating those coordinates in that order.
  */

package object semiauto extends Derivers {

  def deriveResourceEncoderForTrait[A, Out <: jsonapi.encoding.ResourceLike]: TraitResourceEncoderDeriver[A, Out] =
    macro Macros.deriveResourceEncoderForTrait[A, Out]

  def deriveResourceEncoderForCaseClass[A, Out <: jsonapi.encoding.ResourceLike]: CaseClassResourceEncoderDeriver[
    A,
    Out
  ] =
    macro Macros.deriveResourceEncoderForCaseClass[A, Out]

  def deriveResourceDecoderForTrait[In <: jsonapi.decoding.ResourceLike, A]: TraitResourceDecoderDeriver[In, A] =
    macro Macros.deriveResourceDecoderForTrait[In, A]

  def deriveResourceDecoderForCaseClass[In <: jsonapi.decoding.ResourceLike, A]: CaseClassResourceDecoderDeriver[
    In,
    A
  ] =
    macro Macros.deriveResourceDecoderForCaseClass[In, A]

  def deriveResourceCodecForTrait[
      In <: jsonapi.decoding.ResourceLike,
      A,
      Out <: jsonapi.encoding.ResourceLike
  ]: TraitResourceCodecDeriver[In, A, Out] =
    macro Macros.deriveResourceCodecForTrait[In, A, Out]

  def deriveResourceCodecForCaseClass[
      In <: jsonapi.decoding.ResourceLike,
      A,
      Out <: jsonapi.encoding.ResourceLike
  ]: CaseClassResourceCodecDeriver[In, A, Out] =
    macro Macros.deriveResourceCodecForCaseClass[In, A, Out]

  //------------------------------------------------------------------------------------------------------------------
  // These calls below here just fix the resource type parameters from above.

  def deriveResourceIdentifierEncoderForTrait[A]: TraitResourceEncoderDeriver[A, jsonapi.encoding.ResourceIdentifier] =
    macro Macros.deriveResourceEncoderForTrait[A, jsonapi.encoding.ResourceIdentifier]

  def deriveResourceObjectEncoderForTrait[A]: TraitResourceEncoderDeriver[A, jsonapi.encoding.ResourceObject] =
    macro Macros.deriveResourceEncoderForTrait[A, jsonapi.encoding.ResourceObject]

  def deriveResourceObjectOptionalIdEncoderForTrait[A]: TraitResourceEncoderDeriver[
    A,
    jsonapi.encoding.ResourceObjectOptionalId
  ] =
    macro Macros.deriveResourceEncoderForTrait[A, jsonapi.encoding.ResourceObjectOptionalId]

  def deriveResourceIdentifierEncoderForCaseClass[A]: CaseClassResourceEncoderDeriver[
    A,
    jsonapi.encoding.ResourceIdentifier
  ] =
    macro Macros.deriveResourceEncoderForCaseClass[A, jsonapi.encoding.ResourceIdentifier]

  def deriveResourceObjectEncoderForCaseClass[A]: CaseClassResourceEncoderDeriver[A, jsonapi.encoding.ResourceObject] =
    macro Macros.deriveResourceEncoderForCaseClass[A, jsonapi.encoding.ResourceObject]

  def deriveResourceObjectOptionalIdEncoderForCaseClass[A]: CaseClassResourceEncoderDeriver[
    A,
    jsonapi.encoding.ResourceObjectOptionalId
  ] =
    macro Macros.deriveResourceEncoderForCaseClass[A, jsonapi.encoding.ResourceObjectOptionalId]

  def deriveResourceIdentifierDecoderForTrait[A]: TraitResourceDecoderDeriver[jsonapi.decoding.ResourceIdentifier, A] =
    macro Macros.deriveResourceDecoderForTrait[jsonapi.decoding.ResourceIdentifier, A]

  def deriveResourceObjectDecoderForTrait[A]: TraitResourceDecoderDeriver[jsonapi.decoding.ResourceObject, A] =
    macro Macros.deriveResourceDecoderForTrait[jsonapi.decoding.ResourceObject, A]

  def deriveResourceObjectOptionalIdDecoderForTrait[A]: TraitResourceDecoderDeriver[
    jsonapi.decoding.ResourceObjectOptionalId,
    A
  ] =
    macro Macros.deriveResourceDecoderForTrait[jsonapi.decoding.ResourceObjectOptionalId, A]

  def deriveResourceIdentifierDecoderForCaseClass[A]: CaseClassResourceDecoderDeriver[
    jsonapi.decoding.ResourceIdentifier,
    A
  ] =
    macro Macros.deriveResourceDecoderForCaseClass[jsonapi.decoding.ResourceIdentifier, A]

  def deriveResourceObjectDecoderForCaseClass[A]: CaseClassResourceDecoderDeriver[jsonapi.decoding.ResourceObject, A] =
    macro Macros.deriveResourceDecoderForCaseClass[jsonapi.decoding.ResourceObject, A]

  def deriveResourceObjectOptionalIdDecoderForCaseClass[A]: CaseClassResourceDecoderDeriver[
    jsonapi.decoding.ResourceObjectOptionalId,
    A
  ] =
    macro Macros.deriveResourceDecoderForCaseClass[jsonapi.decoding.ResourceObjectOptionalId, A]

  def deriveResourceIdentifierCodecForTrait[A]: TraitResourceCodecDeriver[
    jsonapi.decoding.ResourceIdentifier,
    A,
    jsonapi.encoding.ResourceIdentifier
  ] =
    macro Macros
      .deriveResourceCodecForTrait[jsonapi.decoding.ResourceIdentifier, A, jsonapi.encoding.ResourceIdentifier]

  def deriveResourceObjectCodecForTrait[A]: TraitResourceCodecDeriver[
    jsonapi.decoding.ResourceObject,
    A,
    jsonapi.encoding.ResourceObject
  ] =
    macro Macros.deriveResourceCodecForTrait[jsonapi.decoding.ResourceObject, A, jsonapi.encoding.ResourceObject]

  def deriveResourceObjectOptionalIdCodecForTrait[A]: TraitResourceCodecDeriver[
    jsonapi.decoding.ResourceObjectOptionalId,
    A,
    jsonapi.encoding.ResourceObjectOptionalId
  ] =
    macro Macros.deriveResourceCodecForTrait[
      jsonapi.decoding.ResourceObjectOptionalId,
      A,
      jsonapi.encoding.ResourceObjectOptionalId
    ]

  def deriveResourceIdentifierCodecForCaseClass[A]: CaseClassResourceCodecDeriver[
    jsonapi.decoding.ResourceIdentifier,
    A,
    jsonapi.encoding.ResourceIdentifier
  ] =
    macro Macros
      .deriveResourceCodecForCaseClass[jsonapi.decoding.ResourceIdentifier, A, jsonapi.encoding.ResourceIdentifier]

  def deriveResourceObjectCodecForCaseClass[A]: CaseClassResourceCodecDeriver[
    jsonapi.decoding.ResourceObject,
    A,
    jsonapi.encoding.ResourceObject
  ] =
    macro Macros.deriveResourceCodecForCaseClass[jsonapi.decoding.ResourceObject, A, jsonapi.encoding.ResourceObject]

  def deriveResourceObjectOptionalIdCodecForCaseClass[A]: CaseClassResourceCodecDeriver[
    jsonapi.decoding.ResourceObjectOptionalId,
    A,
    jsonapi.encoding.ResourceObjectOptionalId
  ] =
    macro Macros.deriveResourceCodecForCaseClass[
      jsonapi.decoding.ResourceObjectOptionalId,
      A,
      jsonapi.encoding.ResourceObjectOptionalId
    ]

  object unchecked {

    def deriveResourceEncoderForTrait[A, Out <: jsonapi.encoding.ResourceLike]: TraitResourceEncoderDeriver[A, Out] =
      new TraitResourceEncoderDeriver[A, Out]

    def deriveResourceEncoderForCaseClass[A, Out <: jsonapi.encoding.ResourceLike]
        : CaseClassResourceEncoderDeriver[A, Out] =
      new CaseClassResourceEncoderDeriver[A, Out]

    def deriveResourceDecoderForTrait[In <: jsonapi.decoding.ResourceLike, A]: TraitResourceDecoderDeriver[In, A] =
      new TraitResourceDecoderDeriver[In, A]

    def deriveResourceDecoderForCaseClass[In <: jsonapi.decoding.ResourceLike, A]
        : CaseClassResourceDecoderDeriver[In, A] =
      new CaseClassResourceDecoderDeriver[In, A]

    def deriveResourceCodecForTrait[In <: jsonapi.decoding.ResourceLike, A, Out <: jsonapi.encoding.ResourceLike]
        : TraitResourceCodecDeriver[In, A, Out] =
      new TraitResourceCodecDeriver[In, A, Out]

    def deriveResourceCodecForCaseClass[In <: jsonapi.decoding.ResourceLike, A, Out <: jsonapi.encoding.ResourceLike]
        : CaseClassResourceCodecDeriver[In, A, Out] =
      new CaseClassResourceCodecDeriver[In, A, Out]

    //------------------------------------------------------------------------------------------------------------------
    // These calls below here just fix the resource type parameters from above.

    def deriveResourceIdentifierEncoderForTrait[A]
        : TraitResourceEncoderDeriver[A, jsonapi.encoding.ResourceIdentifier] =
      deriveResourceEncoderForTrait[A, jsonapi.encoding.ResourceIdentifier]

    def deriveResourceObjectEncoderForTrait[A]: TraitResourceEncoderDeriver[A, jsonapi.encoding.ResourceObject] =
      deriveResourceEncoderForTrait[A, jsonapi.encoding.ResourceObject]

    def deriveResourceObjectOptionalIdEncoderForTrait[A]
        : TraitResourceEncoderDeriver[A, jsonapi.encoding.ResourceObjectOptionalId] =
      deriveResourceEncoderForTrait[A, jsonapi.encoding.ResourceObjectOptionalId]

    def deriveResourceIdentifierEncoderForCaseClass[A]
        : CaseClassResourceEncoderDeriver[A, jsonapi.encoding.ResourceIdentifier] =
      deriveResourceEncoderForCaseClass[A, jsonapi.encoding.ResourceIdentifier]

    def deriveResourceObjectEncoderForCaseClass[A]
        : CaseClassResourceEncoderDeriver[A, jsonapi.encoding.ResourceObject] =
      deriveResourceEncoderForCaseClass[A, jsonapi.encoding.ResourceObject]

    def deriveResourceObjectOptionalIdEncoderForCaseClass[A]
        : CaseClassResourceEncoderDeriver[A, jsonapi.encoding.ResourceObjectOptionalId] =
      deriveResourceEncoderForCaseClass[A, jsonapi.encoding.ResourceObjectOptionalId]

    def deriveResourceIdentifierDecoderForTrait[A]
        : TraitResourceDecoderDeriver[jsonapi.decoding.ResourceIdentifier, A] =
      deriveResourceDecoderForTrait[jsonapi.decoding.ResourceIdentifier, A]

    def deriveResourceObjectDecoderForTrait[A]: TraitResourceDecoderDeriver[jsonapi.decoding.ResourceObject, A] =
      deriveResourceDecoderForTrait[jsonapi.decoding.ResourceObject, A]

    def deriveResourceObjectOptionalIdDecoderForTrait[A]
        : TraitResourceDecoderDeriver[jsonapi.decoding.ResourceObjectOptionalId, A] =
      deriveResourceDecoderForTrait[jsonapi.decoding.ResourceObjectOptionalId, A]

    def deriveResourceIdentifierDecoderForCaseClass[A]
        : CaseClassResourceDecoderDeriver[jsonapi.decoding.ResourceIdentifier, A] =
      deriveResourceDecoderForCaseClass[jsonapi.decoding.ResourceIdentifier, A]

    def deriveResourceObjectDecoderForCaseClass[A]
        : CaseClassResourceDecoderDeriver[jsonapi.decoding.ResourceObject, A] =
      deriveResourceDecoderForCaseClass[jsonapi.decoding.ResourceObject, A]

    def deriveResourceObjectOptionalIdDecoderForCaseClass[A]
        : CaseClassResourceDecoderDeriver[jsonapi.decoding.ResourceObjectOptionalId, A] =
      deriveResourceDecoderForCaseClass[jsonapi.decoding.ResourceObjectOptionalId, A]

    def deriveResourceIdentifierCodecForTrait[A]
        : TraitResourceCodecDeriver[jsonapi.decoding.ResourceIdentifier, A, jsonapi.encoding.ResourceIdentifier] =
      deriveResourceCodecForTrait[jsonapi.decoding.ResourceIdentifier, A, jsonapi.encoding.ResourceIdentifier]

    def deriveResourceObjectCodecForTrait[A]
        : TraitResourceCodecDeriver[jsonapi.decoding.ResourceObject, A, jsonapi.encoding.ResourceObject] =
      deriveResourceCodecForTrait[jsonapi.decoding.ResourceObject, A, jsonapi.encoding.ResourceObject]

    def deriveResourceObjectOptionalIdCodecForTrait[A]: TraitResourceCodecDeriver[
      jsonapi.decoding.ResourceObjectOptionalId,
      A,
      jsonapi.encoding.ResourceObjectOptionalId
    ] =
      deriveResourceCodecForTrait[
        jsonapi.decoding.ResourceObjectOptionalId,
        A,
        jsonapi.encoding.ResourceObjectOptionalId
      ]

    def deriveResourceIdentifierCodecForCaseClass[A]
        : CaseClassResourceCodecDeriver[jsonapi.decoding.ResourceIdentifier, A, jsonapi.encoding.ResourceIdentifier] =
      deriveResourceCodecForCaseClass[jsonapi.decoding.ResourceIdentifier, A, jsonapi.encoding.ResourceIdentifier]

    def deriveResourceObjectCodecForCaseClass[A]
        : CaseClassResourceCodecDeriver[jsonapi.decoding.ResourceObject, A, jsonapi.encoding.ResourceObject] =
      deriveResourceCodecForCaseClass[jsonapi.decoding.ResourceObject, A, jsonapi.encoding.ResourceObject]

    def deriveResourceObjectOptionalIdCodecForCaseClass[A]: CaseClassResourceCodecDeriver[
      jsonapi.decoding.ResourceObjectOptionalId,
      A,
      jsonapi.encoding.ResourceObjectOptionalId
    ] =
      deriveResourceCodecForCaseClass[
        jsonapi.decoding.ResourceObjectOptionalId,
        A,
        jsonapi.encoding.ResourceObjectOptionalId
      ]
  }
}
