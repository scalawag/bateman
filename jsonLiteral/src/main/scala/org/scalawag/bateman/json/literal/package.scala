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

package org.scalawag.bateman.json

import org.scalawag.bateman.json.focus.JRootFocus

import scala.language.experimental.macros

package object literal {
  implicit final class JsonStringContext(sc: StringContext) {

    /** Generate a JSON object from a JSON text with optional expression interpolations, which must represent JSON
      * values. That is, no partial values, punctuation or object field keys can be generated through interpolation.
      *
      * This will fail at compile-time if the resulting JSON text is invalid or is not an object.
      *
      * @param args items to be inserted into the text (must have a [[JAnyEncoder]] in scope.
      * @return the JObject resulting from parsing the JSON text with the interpolations performed
      */
    def json(args: Any*): JObject = macro LiteralMacros.jobjectStringContext

    /** Generate a JSON array from a JSON text with optional expression interpolations, which must represent JSON
      * values. That is, no partial values, punctuation or object field keys can be generated through interpolation.
      *
      * This will fail at compile-time if the resulting JSON text is invalid or is not an array.
      *
      * @param args items to be inserted into the text (must have a [[JAnyEncoder]] in scope.
      * @return the JArray resulting from parsing the JSON text with the interpolations performed
      */
    def jsona(args: Any*): JArray = macro LiteralMacros.jarrayStringContext
  }
}
