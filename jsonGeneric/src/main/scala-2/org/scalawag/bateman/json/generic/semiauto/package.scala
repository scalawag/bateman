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

package org.scalawag.bateman.json.generic

import org.scalawag.bateman.json.{JObjectCodec, JObjectDecoder, JObjectEncoder}
import org.scalawag.bateman.json.generic.encoding.CaseClassEncoderFactory
import org.scalawag.bateman.json.generic.decoding.CaseClassDecoderFactory
import org.scalawag.bateman.json.generic.semiauto.Derivers.{
  CaseClassCodecDeriver,
  CaseClassDecoderDeriver,
  CaseClassEncoderDeriver,
  TraitCodecDeriver,
  TraitDecoderDeriver,
  TraitEncoderDeriver
}
import shapeless.Lazy

package object semiauto {
  def deriveEncoderForTrait[A]: TraitEncoderDeriver[A] = new TraitEncoderDeriver[A]
  def deriveEncoderForCaseClass[A]: CaseClassEncoderDeriver[A] = new CaseClassEncoderDeriver[A]
  def deriveDecoderForTrait[A]: TraitDecoderDeriver[A] = new TraitDecoderDeriver[A]
  def deriveDecoderForCaseClass[A]: CaseClassDecoderDeriver[A] = new CaseClassDecoderDeriver[A]
  def deriveCodecForTrait[A]: TraitCodecDeriver[A] = new TraitCodecDeriver[A]
  def deriveCodecForCaseClass[A]: CaseClassCodecDeriver[A] = new CaseClassCodecDeriver[A]

  implicit def caseClassEncoderDeriverToEncoder[A](d: CaseClassEncoderDeriver[A])(implicit
      encoderFactory: Lazy[CaseClassEncoderFactory[A]],
      defaultConfig: Config = Config.default
  ): JObjectEncoder[A] = d()

  implicit def caseClassDecoderDeriverToDecoder[A](d: CaseClassDecoderDeriver[A])(implicit
      decoderFactory: Lazy[CaseClassDecoderFactory[A]],
      defaultConfig: Config = Config.default
  ): JObjectDecoder[A] = d()

  implicit def caseClassCodecDeriverToCodec[A](d: CaseClassCodecDeriver[A])(implicit
      encoderFactory: Lazy[CaseClassEncoderFactory[A]],
      decoderFactory: Lazy[CaseClassDecoderFactory[A]],
      defaultConfig: Config = Config.default
  ): JObjectCodec[A] = d()
}
