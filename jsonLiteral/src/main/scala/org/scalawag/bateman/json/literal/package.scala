package org.scalawag.bateman.json

import scala.language.experimental.macros
import org.scalawag.bateman.json.encoding.JAny

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
    def json(args: Any*): JAny = macro LiteralMacros.jsonStringContext
  }
}
