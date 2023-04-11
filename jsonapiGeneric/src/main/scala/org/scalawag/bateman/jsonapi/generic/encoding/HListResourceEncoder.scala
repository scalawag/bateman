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

package org.scalawag.bateman.jsonapi.generic.encoding

import org.scalawag.bateman.json.JObject
import org.scalawag.bateman.jsonapi.generic.encoding.HListResourceEncoderFactory.Input
import shapeless.HList

trait HListResourceEncoder[In <: HList, Defaults <: HList, Annot <: HList] {
  // Instead of returning a result here, just keep the errors in the PartialResource. This makes it easier to gather
  // all of the errors while processing as much as possible. At the end, we still fail if there are any errors, but
  // we lose some of the fail-fast behavior that happens when there are a bunch of different encoders interacting.
  def encode(input: Input[In], discriminators: JObject): PartialResource
}
