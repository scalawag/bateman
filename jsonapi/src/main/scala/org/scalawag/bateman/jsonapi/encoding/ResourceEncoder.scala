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

package org.scalawag.bateman.jsonapi.encoding

import org.scalawag.bateman.json.{JObject, JObjectEncoder, ProgrammerError}
import org.scalawag.bateman.jsonapi.encoding.ResourceEncoder.Encoded
import org.scalawag.bateman.json.syntax._

trait ResourceEncoder[In] extends JObjectEncoder[In] {
  def encodeResource(
      in: In,
      includeSpec: IncludeSpec = IncludeSpec.Opportunistically,
      fieldsSpec: FieldsSpec = FieldsSpec.All,
      discriminators: JObject = JObject.Empty
  ): EncodeResult[Encoded]

  def encodeInfallibly(
      in: In,
      includeSpec: InfallibleIncludeSpec = IncludeSpec.Opportunistically,
      fieldsSpec: FieldsSpec.Infallible = FieldsSpec.All,
      discriminators: JObject = JObject.Empty
  ): Encoded = {
    val penc = encodeResource(in, includeSpec, fieldsSpec, discriminators).getOrElse(
      throw ProgrammerError("An encoding operation with infallible parameters shouldn't fail!")
    )
    Encoded(penc.root, penc.inclusions)
  }

  def encodeMinimally(in: In, discriminators: JObject = JObject.Empty): JObject =
    encodeInfallibly(in, IncludeSpec.Never, FieldsSpec.None, discriminators).root

  override def encode(in: In, discriminators: JObject): JObject =
    encodeInfallibly(in, IncludeSpec.Never, FieldsSpec.All, discriminators).root
}

object ResourceEncoder {
  def apply[In](implicit enc: ResourceEncoder[In]): ResourceEncoder[In] = enc

  def encodeResource[In](
      in: In,
      includeSpec: IncludeSpec = IncludeSpec.Opportunistically,
      fieldsSpec: FieldsSpec = FieldsSpec.All
  )(implicit enc: ResourceEncoder[In]): EncodeResult[Encoded] = enc.encodeResource(in, includeSpec, fieldsSpec)

  trait EncodedLike {
    val root: JObject
    val inclusions: Inclusions
  }

  case class Encoded(root: JObject, inclusions: Inclusions = Inclusions.empty) extends EncodedLike {
    def map(fn: JObject => JObject): Encoded = copy(root = fn(root))
    def toDocument: JObject = JObject("data" -> root, "included" -> inclusions.objects.toList.toJAny)
  }
}
