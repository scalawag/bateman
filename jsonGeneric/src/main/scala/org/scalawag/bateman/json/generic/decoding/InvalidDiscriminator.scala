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

package org.scalawag.bateman.json.generic.decoding

import org.scalawag.bateman.json.focus.JFocus
import org.scalawag.bateman.json.{JAny, JError, JLocation, JPointer}

case class InvalidDiscriminator(discriminator: JFocus[JAny], valids: Set[JAny]) extends JError {
  override def location: Option[JLocation] = discriminator.value.location
  override def pointer: JPointer = discriminator.pointer
  override lazy val description: String =
    s"discriminator must be one of the following: ${valids.map(_.render).toList.sorted.mkString("|")}"
}
