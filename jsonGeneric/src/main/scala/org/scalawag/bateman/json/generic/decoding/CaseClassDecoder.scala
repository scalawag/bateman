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

package org.scalawag.bateman.json.generic.decoding

import org.scalawag.bateman.json.decoding._
import org.scalawag.bateman.json.generic.decoding.HListDecoderFactoryFactory.Params
import org.scalawag.bateman.json.generic.{CaseClassInfo, Config, HasDiscriminatorValue, MemberLabels}
import shapeless.{Default, Generic, HList}

import scala.reflect.ClassTag

trait CaseClassDecoder[To, Context] extends JObjectContextualDecoder[To, Context] with HasDiscriminatorValue {
  override def decode(in: JObject, context: Context): DecodeResult[To] = decode(in, context, None)

  /** Gives the coproduct decoder the ability to pass in the name of the discriminator field that it used so that
    * the field can be excluded from the extraneous fields error checking.
    */
  def decode(in: JObject, context: Context, discriminatorField: Option[String]): DecodeResult[To]
}

trait CaseClassDecoderFactory[To, Context] {
  def apply(discriminatorValueOverride: Option[String], config: Config): CaseClassDecoder[To, Context]
}

object CaseClassDecoderFactory {

  /** Generates a decoder for a case class according to the configuration specified implicitly.
    *
    * The overall strategy here is to use a [[HListDecoder]], which expects its input to contain both the
    * type/name of the head item as well as the default value from the case class. If the named field is not
    * represented in the incoming JSON object, the default is used, if available. If there is no default, an
    * appropriate error is returned.
    *
    * @param generic instantiates a [[CaseClass]] from its generic representation
    * @param defaults gets the default parameters of [[CaseClass]] as an unlabelled HList of [[Option]]s
    * @param setToNone sets all of the default values to [[None]] in cases where we shouldn't use the defaults
    * @param decoder generates a decoder the generic representation with its defaults
    * @param config configures the behavior of the generated decoder
    * @tparam CaseClass the case class for which we're generating a decoder
    * @tparam Generic the labelled generic representation of [[CaseClass]]
    * @tparam Defaults the generic representation of the defaults as [[Option]]s (unlabelled)
    * @tparam Context the context within which the generated decoder operates
    * @return the generated decoder
    */
  implicit def caseClassDecoder[CaseClass: ClassTag, Generic <: HList, Defaults <: HList, Context](implicit
      generic: Generic.Aux[CaseClass, Generic],
      fieldNames: MemberLabels[CaseClass],
      defaults: Default.AsOptions.Aux[CaseClass, Defaults],
      genericDecoderFactoryFactory: HListDecoderFactoryFactory[Generic, Defaults, Context]
  ): CaseClassDecoderFactory[CaseClass, Context] = {
    val classInfo = CaseClassInfo(defaults(), fieldNames())
    val genericDecoderFactory = genericDecoderFactoryFactory(classInfo)

    (discriminatorValueOverride, config) => {
      val params = Params(config, discriminatorValueOverride)
      val hlistDecoder = genericDecoderFactory(params)

      new CaseClassDecoder[CaseClass, Context] {

        override val discriminatorValue: String = params.discriminatorValueFor[CaseClass]

        override def decode(
            in: JObject,
            context: Context,
            discriminatorField: Option[String]
        ): DecodeResult[CaseClass] = {
          val input = HListDecoderFactoryFactory.Input(in, context, discriminatorField)
          hlistDecoder.decode(input).map(_.out).map(generic.from)
        }
      }
    }
  }
}
