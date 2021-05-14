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

import org.scalawag.bateman.json.generic.naming.CaseTransformation
import org.scalawag.bateman.json.decoding.{UnspecifiedField, UnexpectedValue}

/** Controls the derivation of encoders and decoders. An instance must be available in implicit scope during
  * derivation or else the default configuration will be used.
  *
  * @param fieldNameMapping maps a Scala field name to a JSON field name. [[CaseTransformation]] may come in very
  *                         handy here
  * @param classNameMapping maps a Scala class name to a JSON string value. [[CaseTransformation]] may come in very
  *                         handy here
  * @param discriminatorField the name of the JSON field that distinguishes between concrete classes when a trait is
  *                           encoded or decoded. The value will be the name of the class passed through
  *                           [[classNameMapping]].
  * @param useDefaultsForMissingFields determines whether or not the default arguments declared in the case class are
  *                                    used for keys that are not found in the incoming JSON object. By default, they
  *                                    are used. If this is set to `false`, missing fields will result in
  *                                    [[UnspecifiedField]] errors ''even if'' their types lend themselves to absence
  *                                    (i.e., [[Option]]).
  * @param allowUnknownFields determines whether or not fields appearing in the incoming JSON object that do not have
  *                           corresponding case class fields are ignored. This is the default behavior. If set to
  *                           `false`, unknown fields result in [[UnexpectedValue]] errors being returned.
  * @param encodeDefaultValues determines whether or not case class fields that are set to their default value are
  *                            encoded or not. The default behavior is to leave them out of the resulting JSON object.
  */

case class Config(
    fieldNameMapping: String => String = identity,
    classNameMapping: String => String = identity,
    discriminatorField: String = "type",
    useDefaultsForMissingFields: Boolean = true,
    allowUnknownFields: Boolean = true,
    encodeDefaultValues: Boolean = false
)

object Config {
  val default: Config = Config()

  def get(implicit config: Config = Config.default): Config = config
}
