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

package org.scalawag.bateman.json.decoding

/** Represents a specific location, to the character, in a JSON text.
  *
  * @param line the one-based line of the position in the text
  * @param column the one-based column of the position in the text
  * @param source optional source of the JSON text (e.g., file name, classpath resource name or URL)
  */

final case class JLocation(line: Int, column: Int, source: Option[String] = None) {
  override val toString: String = s"${source.map(x => s"$x:").getOrElse("")}$line:$column"
}

object JLocation {
  implicit val ordering: Ordering[JLocation] = Ordering.by(pos => pos.source -> pos.line -> pos.column)
}
