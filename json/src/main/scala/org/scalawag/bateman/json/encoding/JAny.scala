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

package org.scalawag.bateman.json.encoding

sealed trait JAny {
  def render: String = render(NoSpacesRenderer)
  def spaces2: String = render(PrettySpaces2)
  def spaces4: String = render(PrettySpaces4)
  def render(renderer: Renderer): String = renderer.render(this)
}

sealed trait JNull extends JAny
case object JNull extends JNull

case class JString(value: String) extends JAny

case class JBoolean(value: Boolean) extends JAny

case class JNumber private (value: String) extends JAny {}

case object JNumber {
  private val re = """-?(?:0|[1-9]\d*)(?:\.\d+)?(?:[eE][+-]?\d+)?""".r

  def apply(n: String): Either[String, JNumber] =
    n match {
      case re() => Right(new JNumber(n))
      case _    => Left(s"not a valid JSON number: $n")
    }

  def unsafe(n: String): JNumber = JNumber(n).fold(e => throw new IllegalArgumentException(e), identity)
  def apply(l: Long): JNumber = unsafe(l.toString)
  def apply(d: Double): JNumber = unsafe(d.toString)
  def apply(i: BigInt): JNumber = unsafe(i.toString)
  def apply(d: BigDecimal): JNumber = unsafe(d.toString)

  def unapply(n: JNumber): Option[String] = Some(n.value)
}

final case class JArray(items: JAny*) extends JAny {
  def unapply(arr: JArray): Option[List[JAny]] = Some(arr.items.toList)
}

final case class JObject(fields: (String, JAny)*) extends JAny

case object JObject {
  def fromOptions(fields: Iterable[(String, JAny)]*): JObject = apply(fields.flatten: _*)
}
