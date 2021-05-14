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

package org.scalawag.bateman.json.generic

import scala.language.experimental.macros
import org.scalawag.bateman.json.generic.semiauto.Derivers.{
  CaseClassCodecDeriver,
  CaseClassDecoderDeriver,
  CaseClassEncoderDeriver,
  TraitCodecDeriver,
  TraitDecoderDeriver,
  TraitEncoderDeriver
}

package object semiauto {
  // These all do essentially the same thing as the unchecked versions (below) except that they run the type through
  // a macro which does some extra validation on it to detect compile-time errors. They also attempt to check for the
  // codec dependencies individually to get the programmer a more targeted error message when they're missing one.
  def deriveEncoderForTrait[A]: TraitEncoderDeriver[A] =
    macro Macros.deriveEncoderForTrait[A]
  def deriveEncoderForCaseClass[A]: CaseClassEncoderDeriver[A] =
    macro Macros.deriveEncoderForCaseClass[A]
  def deriveDecoderForTrait[A, Context]: TraitDecoderDeriver[A, Context] =
    macro Macros.deriveDecoderForTrait[A, Context]
  def deriveDecoderForCaseClass[A, Context]: CaseClassDecoderDeriver[A, Context] =
    macro Macros.deriveDecoderForCaseClass[A, Context]
  def deriveCodecForTrait[A, Context]: TraitCodecDeriver[A, Context] =
    macro Macros.deriveCodecForTrait[A, Context]
  def deriveCodecForCaseClass[A, Context]: CaseClassCodecDeriver[A, Context] =
    macro Macros.deriveCodecForCaseClass[A, Context]

  object unchecked {
    def deriveEncoderForTrait[A]: TraitEncoderDeriver[A] =
      new TraitEncoderDeriver[A]
    def deriveEncoderForCaseClass[A]: CaseClassEncoderDeriver[A] =
      new CaseClassEncoderDeriver[A]
    def deriveDecoderForTrait[A, Context]: TraitDecoderDeriver[A, Context] =
      new TraitDecoderDeriver[A, Context]
    def deriveDecoderForCaseClass[A, Context]: CaseClassDecoderDeriver[A, Context] =
      new CaseClassDecoderDeriver[A, Context]
    def deriveCodecForTrait[A, Context]: TraitCodecDeriver[A, Context] =
      new TraitCodecDeriver[A, Context]
    def deriveCodecForCaseClass[A, Context]: CaseClassCodecDeriver[A, Context] =
      new CaseClassCodecDeriver[A, Context]
  }
}
