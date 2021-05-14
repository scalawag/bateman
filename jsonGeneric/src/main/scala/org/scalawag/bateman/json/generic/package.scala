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

package org.scalawag.bateman.json

import org.scalawag.bateman.json.validating.{ValidationResult, Validator}
import shapeless.tag.@@

package object generic {

  type TaggedValidator[A, B] = Validator[A, A @@ B]

  object TaggedValidator {
    def apply[A, B](implicit validator: TaggedValidator[A, B]): TaggedValidator[A, B] = validator

    class AnchoredTaggedValidator[B] {
      def validate[A](a: A)(implicit validator: TaggedValidator[A, B]): ValidationResult[A @@ B] =
        validator.validate(a)
    }

    def apply[B]: AnchoredTaggedValidator[B] = new AnchoredTaggedValidator[B]
  }

}
