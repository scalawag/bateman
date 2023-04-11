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

package org.scalawag.bateman.jsonapi.generic

import org.scalawag.bateman.json.JAny

import scala.reflect.runtime.universe.TypeTag
import scala.language.experimental.macros
import org.scalawag.bateman.jsonapi

/** Contains every combination of a two-dimensional space.
  *
  * CaseClass/Trait
  * Decoder/Encoder/Codec
  *
  * Names are created by concatenating those coordinates in that order.
  */

package object semiauto extends Derivers {

//  def deriveResourceEncoderForTrait[A, Out <: jsonapi.encoding.ResourceLike]: TraitResourceEncoderDeriver[A, Out] =
//    macro Macros.deriveResourceEncoderForTrait[A, Out]
//
//  def deriveResourceEncoderForCaseClass[A, Out <: jsonapi.encoding.ResourceLike]: CaseClassResourceEncoderDeriver[
//    A,
//    Out
//  ] =
//    macro Macros.deriveResourceEncoderForCaseClass[A, Out]
//
//  def deriveResourceDecoderForTrait[In <: jsonapi.decoding.ResourceLike, A]: TraitResourceDecoderDeriver[In, A] =
//    macro Macros.deriveResourceDecoderForTrait[In, A]
//
//  def deriveResourceDecoderForCaseClass[In <: jsonapi.decoding.ResourceLike, A]: CaseClassResourceDecoderDeriver[
//    In,
//    A
//  ] =
//    macro Macros.deriveResourceDecoderForCaseClass[In, A]
//
//  def deriveResourceCodecForTrait[
//      In <: jsonapi.decoding.ResourceLike,
//      A,
//      Out <: jsonapi.encoding.ResourceLike
//  ]: TraitResourceCodecDeriver[In, A, Out] =
//    macro Macros.deriveResourceCodecForTrait[In, A, Out]
//
//  def deriveResourceCodecForCaseClass[
//      In <: jsonapi.decoding.ResourceLike,
//      A,
//      Out <: jsonapi.encoding.ResourceLike
//  ]: CaseClassResourceCodecDeriver[In, A, Out] =
//    macro Macros.deriveResourceCodecForCaseClass[In, A, Out]
//

  object unchecked {
    def deriveResourceEncoderForTrait[A]: TraitResourceEncoderDeriver[A] =
      new TraitResourceEncoderDeriver[A]

    def deriveResourceEncoderForCaseClass[A]: CaseClassResourceEncoderDeriver[A] =
      new CaseClassResourceEncoderDeriver[A]

    def deriveResourceDecoderForTrait[A]: TraitResourceDecoderDeriver[A] =
      new TraitResourceDecoderDeriver[A]

    def deriveResourceDecoderForCaseClass[A]: CaseClassResourceDecoderDeriver[A] =
      new CaseClassResourceDecoderDeriver[A]

    def deriveResourceCodecForTrait[A]: TraitResourceCodecDeriver[A] =
      new TraitResourceCodecDeriver[A]

    def deriveResourceCodecForCaseClass[A]: CaseClassResourceCodecDeriver[A] =
      new CaseClassResourceCodecDeriver[A]
  }
}
