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

package org.scalawag.bateman.jsonapi

import org.scalawag.bateman.json.decoding.ContextualDecoder

package object decoding {
  type ResourceDecoder[In, Out] = ContextualDecoder[In, Out, Document]

  type ResourceIdentifierDecoder[A] = ResourceDecoder[ResourceIdentifier, A]

  object ResourceIdentifierDecoder {
    def apply[A](implicit dec: ResourceDecoder[ResourceIdentifier, A]): ResourceIdentifierDecoder[A] = dec
  }

  type ResourceObjectDecoder[A] = ResourceDecoder[ResourceObject, A]

  object ResourceObjectDecoder {
    def apply[A](implicit dec: ResourceDecoder[ResourceObject, A]): ResourceObjectDecoder[A] = dec
  }

  type ResourceObjectOptionalIdDecoder[A] = ResourceDecoder[ResourceObjectOptionalId, A]

  object ResourceObjectOptionalIdDecoder {
    def apply[A](implicit dec: ResourceDecoder[ResourceObjectOptionalId, A]): ResourceObjectOptionalIdDecoder[A] = dec
  }

}
