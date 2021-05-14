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

package org.scalawag.bateman.json.circe

import io.circe.Json
import org.scalawag.bateman.json.encoding.JAny

object syntax {

  implicit class ToCirceSyntax(in: JAny) {
    def toCirce: Json = CirceConversions.toCirce(in)
  }

  implicit class FromCirceSyntax(in: Json) {
    def toBateman: JAny = CirceConversions.fromCirce(in)
  }

}
