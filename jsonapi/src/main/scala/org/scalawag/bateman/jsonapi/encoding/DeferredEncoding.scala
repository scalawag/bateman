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

package org.scalawag.bateman.jsonapi.encoding

import org.scalawag.bateman.jsonapi.encoding.ResourceEncoder.PartiallyEncoded

/** Represents an object that needs to be encoded and included in the final document based on the inclusion rules. */

case class DeferredEncoding(
    identifier: ResourceIdentifier,
    includeSpec: IncludeSpec.Always,
    fieldsSpec: FieldsSpec
) {
  // Encapsulate this here because otherwise Scala wont believe that encoder corresponds to the instance type when
  // this comes out of a polymorphic list.
//  def encode: Option[EncodeResult[Encoded[ResourceObject]]] = instance.map(encoder.encode(_, includeSpec, fieldsSpec))
//  def encode(from: From): EncodeResult[Encoded[ResourceObject]] = encoder.encode(from, includeSpec, fieldsSpec)
}
