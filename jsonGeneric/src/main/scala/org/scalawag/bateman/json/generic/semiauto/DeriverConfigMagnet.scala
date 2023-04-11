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

package org.scalawag.bateman.json.generic.semiauto

import org.scalawag.bateman.json.generic.Config

/** Determines how a deriver uses the implicit config that's in scope. */
sealed trait DeriverConfigMagnet {
  def apply(config: Config): Config
}

object DeriverConfigMagnet {
  implicit def fromConfig(c: Config): Replace = Replace(c)
  implicit def fromTransform(fn: Config => Config): Transform = Transform(fn)
}

/** Replace the implicit config with the one specified here. */
case class Replace(config: Config) extends DeriverConfigMagnet {
  override def apply(config: Config): Config = this.config
}

/** Defer to the implicit config. */
case object Defer extends DeriverConfigMagnet {
  override def apply(config: Config): Config = config
}

/** Transform the implicit config with the specified function. */
case class Transform(fn: Config => Config) extends DeriverConfigMagnet {
  override def apply(config: Config): Config = fn(config)
}
