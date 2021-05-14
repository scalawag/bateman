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

package org.scalawag.bateman.json.generic.encoding

import org.scalawag.bateman.json.encoding.{JObject, JObjectEncoder}
import org.scalawag.bateman.json.generic.{CaseClassInfo, Config, HasDiscriminatorValue, MemberLabels}
import org.scalawag.bateman.json.generic.encoding.HListEncoderFactoryFactory.{Input, Params}
import shapeless.{Default, Generic, HList}

import scala.reflect.ClassTag

trait CaseClassEncoder[A] extends JObjectEncoder[A] with HasDiscriminatorValue

trait CaseClassEncoderFactory[CaseClass] {
  def apply(discriminatorFieldOverride: Option[String], config: Config): CaseClassEncoder[CaseClass]
}

object CaseClassEncoderFactory {

  /** Generates an encoder for a case class, provided all the fields have encoders.
    *
    * @param chooseAllValues        chooses all values in the input instance
    * @param chooseNonDefaultValues chooses values in the input instance that are not set to their default value
    * @param genericEncoder         generates an encoder for the generic (and optional) representation of this case class
    * @param config                 configures how we generate the encoder
    * @tparam CaseClass the case class to generate an encoder for
    * @tparam Generic    the generic shape of the case class with all the fields wrapped in options
    * @return
    */

  implicit def caseClassEncoder[CaseClass: ClassTag, Defaults <: HList, Generic <: HList](implicit
      generic: Generic.Aux[CaseClass, Generic],
      fieldNames: MemberLabels[CaseClass],
      defaults: Default.AsOptions.Aux[CaseClass, Defaults],
      genericEncoderFactoryFactory: HListEncoderFactoryFactory[Generic, Defaults],
  ): CaseClassEncoderFactory[CaseClass] = {
    val info = CaseClassInfo(defaults(), fieldNames())
    val genericEncoderFactory = genericEncoderFactoryFactory(info)

    (discriminatorFieldOverride, config) => {
      val params = Params(config, discriminatorFieldOverride)
      val genericEncoder = genericEncoderFactory(params)

      new CaseClassEncoder[CaseClass] {
        override def discriminatorValue: String = params.discriminatorValueFor[CaseClass]

        override def encode(a: CaseClass): JObject = {
          val gen = generic.to(a)
          genericEncoder.encode(Input(gen))
        }
      }
    }
  }
}
