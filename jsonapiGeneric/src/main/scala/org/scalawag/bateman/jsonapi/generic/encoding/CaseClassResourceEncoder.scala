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

package org.scalawag.bateman.jsonapi.generic.encoding

import org.scalawag.bateman.json.generic.{CaseClassInfo, Config, MemberLabels}
import org.scalawag.bateman.json.noneIfEmpty
import org.scalawag.bateman.jsonapi.encoding.ResourceEncoder.PartiallyEncoded
import org.scalawag.bateman.jsonapi.encoding._
import org.scalawag.bateman.jsonapi.generic._
import org.scalawag.bateman.jsonapi.generic.encoding.HListResourceEncoderFactoryFactory.{Input, Params}
import shapeless.{Default, Generic, HList}

import scala.reflect.ClassTag

trait CaseClassResourceEncoder[In, +Out <: ResourceLike] extends ResourceEncoder[In, Out] {
  def resourceType: String
}

object CaseClassResourceEncoder {
  def apply[In, Out <: ResourceLike](implicit
      enc: CaseClassResourceEncoder[In, Out]
  ): CaseClassResourceEncoder[In, Out] = enc
}

trait CaseClassResourceEncoderFactory[In, Out <: ResourceLike] {
  def apply(resourceTypeOverride: Option[String], config: Config): CaseClassResourceEncoder[In, Out]
}

object CaseClassResourceEncoderFactory {

  /** Generates a ResourceObjectOptionalIdEncoder for a case class. This generator depends on some constraints that are
    * checked by its corresponding macro [[semiauto.deriveResourceObjectOptionalIdEncoderForCaseClass]].
    *
    * @param labelledGeneric generates the Labelled representation from CaseClass
    * @param partialEncoder generates a PartialResourceEncoder to use for Labelled
    * @param config configuration for the generated encoder (this is the only user-supplied implicit)
    * @tparam CaseClass the case class for which we're generating a ResourceObjectOptionalIdDecoder
    * @tparam Labelled the generic representation of this class as a labelled HList
    * @return a factory which requires the resource type (String) to produce a ResourceObjectOptionalIdDecoder
    */

  implicit def resourceObjectOptionalIdEncoder[CaseClass: ClassTag, Defaults <: HList, Generic <: HList](implicit
      generic: Generic.Aux[CaseClass, Generic],
      fieldNames: MemberLabels[CaseClass],
      defaults: Default.AsOptions.Aux[CaseClass, Defaults],
      genericEncoderFactoryFactory: HListResourceEncoderFactoryFactory[Generic, Defaults],
  ): CaseClassResourceEncoderFactory[CaseClass, ResourceObjectOptionalId] =
    partialResourceEncoder[ResourceObjectOptionalId, CaseClass, Defaults, Generic] { (resourceType, partial) =>
      ResourceObjectOptionalId(
        resourceType,
        partial.id,
        attributes = noneIfEmpty(partial.attributes),
        relationships = noneIfEmpty(partial.relationships),
        meta = noneIfEmpty(partial.metas),
        None
      )
    }

  /** Generates a ResourceObjectEncoder for a case class. This generator depends on some constraints that are
    * checked by its corresponding macro [[semiauto.deriveResourceObjectEncoderForCaseClass]].
    *
    * @param labelledGeneric generates the Labelled representation from CaseClass
    * @param partialEncoder generates a PartialResourceEncoder to use for Labelled
    * @param config configuration for the generated encoder (this is the only user-supplied implicit)
    * @tparam CaseClass the case class for which we're generating a ResourceObjectDecoder
    * @tparam Labelled the generic representation of this class as a labelled HList
    * @tparam Defaulted the case class fields that have default parameters
    * @tparam Undefaulted the case class fields that _don't_ have default parameters
    * @tparam Unaligned a transitory HList of the fields after reassembly, before realigning
    * @return a factory which requires the resource type (String) to produce a ResourceObjectDecoder
    */

  implicit def resourceObjectEncoder[CaseClass: ClassTag, Defaults <: HList, Generic <: HList](implicit
      generic: Generic.Aux[CaseClass, Generic],
      fieldNames: MemberLabels[CaseClass],
      defaults: Default.AsOptions.Aux[CaseClass, Defaults],
      genericEncoderFactoryFactory: HListResourceEncoderFactoryFactory[Generic, Defaults],
  ): CaseClassResourceEncoderFactory[CaseClass, ResourceObject] =
    partialResourceEncoder[ResourceObject, CaseClass, Defaults, Generic] { (resourceType, partial) =>
      ResourceObject(
        resourceType,
        partial.id.get, // Our macro makes sure this is safe. The case class must have a required id field.
        attributes = noneIfEmpty(partial.attributes),
        relationships = noneIfEmpty(partial.relationships),
        meta = noneIfEmpty(partial.metas),
        None
      )
    }

  /** Generates a ResourceIdentifierEncoder for a case class. This generator depends on some constraints that are
    * checked by its corresponding macro [[semiauto.deriveResourceIdentifierEncoderForCaseClass]].
    *
    * @param labelledGeneric generates the Labelled representation from CaseClass
    * @param partialEncoder generates a PartialResourceEncoder to use for Labelled
    * @param config configuration for the generated encoder (this is the only user-supplied implicit)
    * @tparam CaseClass the case class for which we're generating a ResourceIdentifierDecoder
    * @tparam Labelled the generic representation of this class as a labelled HList
    * @return a factory which requires the resource type (String) to produce a ResourceIdentifierDecoder
    */

  implicit def resourceIdentifierEncoder[CaseClass: ClassTag, Defaults <: HList, Generic <: HList](implicit
      generic: Generic.Aux[CaseClass, Generic],
      fieldNames: MemberLabels[CaseClass],
      defaults: Default.AsOptions.Aux[CaseClass, Defaults],
      genericEncoderFactoryFactory: HListResourceEncoderFactoryFactory[Generic, Defaults],
  ): CaseClassResourceEncoderFactory[CaseClass, ResourceIdentifier] =
    partialResourceEncoder[ResourceIdentifier, CaseClass, Defaults, Generic] { (resourceType, partial) =>
      ResourceIdentifier(
        `type` = resourceType,
        id = partial.id.get, // Our macro makes sure this is safe. The case class must have a required id field.
        meta = noneIfEmpty(partial.metas),
      )
    }

  def partialResourceEncoder[Out <: ResourceLike, CaseClass: ClassTag, Defaults <: HList, Generic <: HList](
      fn: (String, PartialResource) => Out
  )(implicit
      generic: Generic.Aux[CaseClass, Generic],
      fieldNames: MemberLabels[CaseClass],
      defaults: Default.AsOptions.Aux[CaseClass, Defaults],
      genericEncoderFactoryFactory: HListResourceEncoderFactoryFactory[Generic, Defaults],
  ): CaseClassResourceEncoderFactory[CaseClass, Out] = {
    val info = CaseClassInfo(defaults(), fieldNames())
    val genericEncoderFactory = genericEncoderFactoryFactory(info)

    (resourceTypeOverride, config) => {
      val params = Params(config, resourceTypeOverride)
      val paramResourceType = params.resourceTypeFor[CaseClass]
      val genericEncoder = genericEncoderFactory(params)

      new CaseClassResourceEncoder[CaseClass, Out] {
        override val resourceType: String = paramResourceType

        override def encodeResource(
            in: CaseClass,
            includeSpec: IncludeSpec,
            fieldsSpec: FieldsSpec
        ): EncodeResult[PartiallyEncoded[Out]] = {
          val input = Input(generic.to(in), resourceType, includeSpec, fieldsSpec)
          // Use the generic encoder to get a PartialResource and use that to create our target type, if there are no errors.
          genericEncoder.encode(input).toEncodeResult.map { partial =>
            PartiallyEncoded(
              fn(resourceType, partial),
              partial.inclusions,
              partial.deferrals
            )
          }
        }
      }
    }
  }

}
