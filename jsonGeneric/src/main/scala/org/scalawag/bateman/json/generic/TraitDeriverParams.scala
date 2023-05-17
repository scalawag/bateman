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

package org.scalawag.bateman.json.generic

import org.scalawag.bateman.json.{JAny, JObject, ProgrammerError}
import org.scalawag.bateman.json.lens.CreatableJLens
import org.scalawag.bateman.json.focus.weak._
import org.scalawag.bateman.json.generic.Discriminators.Discriminator

import scala.reflect.{ClassTag, classTag}

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

  // Adds a discriminator as the first field of the object. This is just my personal preference
  // (as opposed to the last).
  def addDiscriminator[A: ClassTag](base: JObject, value: JAny): JObject = {
    val a = classTag[A]
    base.asRootFocus(discriminatorLens.?).map(_.foci).map {
      case Some(df) if df.value == base =>
        throw ProgrammerError(s"""
          |discriminator issue...
          |The discriminator is not set to a child of the focus, but the focus itself.
          |Change your discriminator lens to focus on a value contained within the object.
        """.trim.stripMargin)
      case Some(df) if df.value.stripLocation != value.stripLocation =>
        throw ProgrammerError(s"""
          |discriminator conflict...
          |The concrete encoder has already encoded something in the location of the discriminator.
          |This is not necessarily a problem except that it has a different value than what the
          |abstract encoder wants to set it to.
          |  concrete type: $a
          |  discriminator: $discriminatorLens
          |  concrete value: ${df.value.render}
          |  abstract value: ${value.render}
        """.trim.stripMargin)
      case _ =>
    }

    // TODO: Make this impossible to fail somehow? The discriminator could be "focus" which would change
    //       it from an object into something else.
    base.asRootFocus.overwriteTo(discriminatorLens, value, prepend = true).root.asObject.map(_.value).getOrThrow
  }
}
