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

package org.scalawag.bateman

import org.scalawag.bateman.json._
import org.scalawag.bateman.jsonapi.encoding.{Link, Relationship, ResourceObject}

package object jsonapi {
  type Included = Seq[ResourceObject]
  type Meta = Seq[(String, JAny)]
  type Links = Seq[(String, Link)]
  type Errors = Seq[encoding.Error]
  type Attributes = Seq[(String, JAny)]
  type Relationships = Seq[(String, Relationship)]
}
