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

import scala.language.experimental.macros
import org.scalawag.bateman.json.encoding.{JAny, JArray, JObject}

package object literal {
  implicit final class JsonStringContext(sc: StringContext) {

    /** Generate an encoding JAny for a JSON text with optional expression interpolations, which must represent JSON
      * values. That is, no partial values, punctuation or object field keys can be generated through interpolation.
      *
      * This will fail at compile-time if the resulting JSON text is invalid.
      *
      * @param args items to be inserted into the text (must have a [[JAnyEncoder]] in scope.
      * @return the JAny resulting from parsing the JSON text with the interpolations performed
      */
    @deprecated("use jany instead", "0.1.11")
    def json(args: Any*): JAny = macro LiteralMacros.janyStringContext

    /** Generate an encoding JAny for a JSON text with optional expression interpolations, which must represent JSON
      * values. That is, no partial values, punctuation or object field keys can be generated through interpolation.
      *
      * This will fail at compile-time if the resulting JSON text is invalid.
      *
      * @param args items to be inserted into the text (must have a [[JAnyEncoder]] in scope.
      * @return the JAny resulting from parsing the JSON text with the interpolations performed
      */
    def jany(args: Any*): JAny = macro LiteralMacros.janyStringContext

    /** Generate an encoding JAny for a JSON text with optional expression interpolations, which must represent JSON
      * values. That is, no partial values, punctuation or object field keys can be generated through interpolation.
      *
      * This will fail at compile-time if the resulting JSON text is invalid.
      *
      * @param args items to be inserted into the text (must have a [[JAnyEncoder]] in scope.
      * @return the JObject resulting from parsing the JSON text with the interpolations performed
      */
    def jobject(args: Any*): JObject = macro LiteralMacros.jobjectStringContext

    /** Generate an encoding JArray for a JSON text with optional expression interpolations, which must represent JSON
      * values. That is, no partial values, punctuation or object field keys can be generated through interpolation.
      *
      * This will fail at compile-time if the resulting JSON text is invalid.
      *
      * @param args items to be inserted into the text (must have a [[JAnyEncoder]] in scope.
      * @return the JArray resulting from parsing the JSON text with the interpolations performed
      */
    def jarray(args: Any*): JArray = macro LiteralMacros.jarrayStringContext
  }
}
