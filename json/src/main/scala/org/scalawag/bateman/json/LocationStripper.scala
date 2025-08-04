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

package org.scalawag.bateman.json

trait LocationStripper[A <: JAny] {
  def stripLocation(a: A): A
}

object LocationStripper extends LocationStripperLowPriority {
  implicit val forJNull: LocationStripper[JNull] = _ => JNull

  implicit val forJString: LocationStripper[JString] = a => if (a.location.isEmpty) a else a.copy(location = None)

  implicit val forJNumber: LocationStripper[JNumber] = a => if (a.location.isEmpty) a else a.copy(location = None)

  implicit val forJBoolean: LocationStripper[JBoolean] = a => if (a.location.isEmpty) a else a.copy(location = None)

  implicit val forJArray: LocationStripper[JArray] = a =>
    if (a.location.isEmpty) a
    else a.copy(items = a.items.map(forJAny.stripLocation), location = None)

  implicit val forJObject: LocationStripper[JObject] = a =>
    if (a.location.isEmpty) a
    else
      a.copy(
        fieldList = a.fieldList.map(f => JField(forJString.stripLocation(f.name), forJAny.stripLocation(f.value))),
        location = None
      )
}

trait LocationStripperLowPriority {
  implicit val forJAny: LocationStripper[JAny] = {
    case v: JNull    => LocationStripper.forJNull.stripLocation(v)
    case v: JString  => LocationStripper.forJString.stripLocation(v)
    case v: JNumber  => LocationStripper.forJNumber.stripLocation(v)
    case v: JBoolean => LocationStripper.forJBoolean.stripLocation(v)
    case v: JArray   => LocationStripper.forJArray.stripLocation(v)
    case v: JObject  => LocationStripper.forJObject.stripLocation(v)
  }
}
