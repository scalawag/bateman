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

package object literal {
  extension (inline sc: StringContext) {

    /** Generate a JSON value from a JSON text with optional expression interpolations, which must represent JSON
      * values. That is, no partial values, punctuation or object field keys can be generated through interpolation.
      *
      * This will fail at compile-time if the resulting JSON text is invalid.
      *
      * @param args items to be inserted into the text (must have a [[JAnyEncoder]] in scope.
      * @return the JSON value resulting from parsing the JSON text with the interpolations performed
      */
    transparent inline def json(inline args: Any*): JAny = LiteralMacros.stringContext(sc)(args*)

    /** Generate a JSON value from a JSON text with optional expression interpolations, which must represent JSON
     * values. That is, no partial values, punctuation or object field keys can be generated through interpolation.
     *
     * This will fail at compile-time if the resulting JSON text is invalid.
     * 
     * (This is the same implementation as the [[json]] string literal and is just here to ease migration from Scala 2
     * to Scala 3. For Scala 3 code, you should use the [[json]] string literal.)
     *
     * @param args items to be inserted into the text (must have a [[JAnyEncoder]] in scope.
     * @return the JSON value resulting from parsing the JSON text with the interpolations performed
     */
    @deprecated("Use json. This is for backward-compatibility with the Scala 2 version only.")
    transparent inline def jsona(inline args: Any*): JAny = LiteralMacros.stringContext(sc)(args*)
  }
}