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

import org.scalawag.bateman.json.{JAny, JObject, ProgrammerError, RichJResult}
import org.scalawag.bateman.json.lens.{CreatableJLens, CreatableJLensOps}
import org.scalawag.bateman.json.generic.Discriminators.Discriminator

/** Contains all the arguments provided to a trait's encoder/decoder deriver at creation time.
  *
  * @param config the configuration to be used for this codec factory
  * @param discriminatorLens the lens to the discriminator field
  * @param discriminator maps from concrete class to discriminator value (and possibly type class instance)
  */
final case class TraitDeriverParams[F[_]](
    config: Config,
    discriminatorLens: CreatableJLens[JObject, JAny],
    discriminator: Discriminator[F]
) {
  implicit val implicitConfig: Config = config

  /** Accumulates a discriminator value into the discriminators JObject. Each trait level in the hierarchy
    * calls this to add its discriminator, building up a JObject that the leaf encoder will merge with the
    * encoded fields. Uses the discriminator lens to write at the correct path (supporting nested
    * discriminators like `meta.status`).
    */
  def addDiscriminator(discriminators: JObject, value: JAny): JObject = {
    // Check for focus-is-root (the lens targets the object itself, not a child)
    discriminators.asRootFocus(discriminatorLens.?).map(_.foci).map {
      case Some(df) if df.value == discriminators =>
        throw ProgrammerError(s"""
          |discriminator issue...
          |The discriminator is not set to a child of the focus, but the focus itself.
          |Change your discriminator lens to focus on a value contained within the object.
        """.trim.stripMargin)
      case _ =>
    }
    discriminators.asRootFocus.encodeTo(discriminatorLens, value, overwrite = true).getOrThrow.value
  }
}
