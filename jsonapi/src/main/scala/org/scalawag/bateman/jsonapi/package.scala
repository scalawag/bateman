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

package org.scalawag.bateman

package object jsonapi {
  type ResourceIdentifierCodec[A] = ResourceCodec[decoding.ResourceIdentifier, A, encoding.ResourceIdentifier]

  object ResourceIdentifierCodec {
    def apply[A](implicit
        codec: ResourceCodec[decoding.ResourceIdentifier, A, encoding.ResourceIdentifier]
    ): ResourceIdentifierCodec[A] = codec
  }

  type ResourceObjectCodec[A] = ResourceCodec[decoding.ResourceObject, A, encoding.ResourceObject]

  object ResourceObjectCodec {
    def apply[A](implicit
        codec: ResourceCodec[decoding.ResourceObject, A, encoding.ResourceObject]
    ): ResourceObjectCodec[A] = codec
  }

  type ResourceObjectOptionalIdCodec[A] =
    ResourceCodec[decoding.ResourceObjectOptionalId, A, encoding.ResourceObjectOptionalId]

  object ResourceObjectOptionalIdCodec {
    def apply[A](implicit
        codec: ResourceCodec[decoding.ResourceObjectOptionalId, A, encoding.ResourceObjectOptionalId]
    ): ResourceObjectOptionalIdCodec[A] = codec
  }

}
