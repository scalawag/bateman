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

package org.scalawag.bateman.jsonapi.generic

package object semiauto extends Derivers {

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

  // Kept for backward compatibility with existing imports of `semiauto._`
  val unchecked: this.type = this
}
