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

package org.scalawag.bateman.json.focus

import scala.language.{higherKinds, implicitConversions}
import org.scalawag.bateman.json._
import org.scalawag.bateman.json.lens.{JLens, ListJLens}

//======================================================================================================================

/** A focus on a particular JSON value within another JSON value as well as an interpreted aspect of that value.
  *
  * @tparam A the type of the focus' value
  */

sealed trait JFocus[+A <: JAny] {

  /** The JSON value that is in focus. */
  def value: A

  /** A JSON Pointer to the value in focus.
    *
    * Note that the JSON specification allows multiple fields within the same object to have the same key. Due to
    * this (and if your document contains such duplicates), the JSON Pointer may not unambiguously identify a
    * single value in the document.
    */
  def pointer: JPointer

  /** A focus to the parent of the value in focus (its containing JSON object or array), if there is such
    * a value. A root focus does not have a parent and will always return [[scala.None]].
    */
  def parentOption: Option[JFocus[JAny]]
}

object JFocus {

  object Value {
    def unapply(focus: JFocus[JAny]): Option[JAny] = Some(focus.value)
  }

  implicit class RichJFocus[F[+_], A <: JAny](me: JFocus[A]) {
    def apply[B <: JAny](op: JLens[F, A, B]): JResult[F[JFocus[B]]] = op(me)
    def apply[B <: JAny](op: ListJLens[A, B]): JResult[JFoci[B]] = op(me)
  }
}

//======================================================================================================================

/** Represents all strongly-typed foci. Anything that derives from this should have a strong static type. */

sealed trait JStrongFocus[+A <: JAny] extends JFocus[A]

object JStrongFocus {
  implicit final def toJFocusOps[A <: JStrongFocus[JAny], B <: JAny](in: A)(implicit
      valueFinder: ValueFinder.Aux[A, B]
  ): JFocusOps[A, B] = new JFocusOps(in)

  implicit final def toJFocusJObjectOps[A <: JStrongFocus[JObject]](in: A): JFocusJObjectOps[A] =
    new JFocusJObjectOps(in)

  implicit final def toJFocusJArrayOps[A <: JStrongFocus[JArray]](in: A): JFocusJArrayOps[A] =
    new JFocusJArrayOps(in)
}

//======================================================================================================================

final case class JRootFocus[+A <: JAny] private[json] (value: A) extends JStrongFocus[A] {
  override val pointer: JPointer = JPointer.Root
  override def parentOption: Option[JFocus[JAny]] = None
}

object JRootFocus {
  implicit final def toJRootFocusOps[A <: JAny](in: JRootFocus[A]): JRootFocusOps[A] = new JRootFocusOps(in)
}

//======================================================================================================================

sealed trait JChildFocus[+A <: JAny, +P <: JFocus[JAny]] extends JStrongFocus[A] {
  val parent: P
  override def parentOption: Option[JFocus[JAny]] = Some(parent)
}

//======================================================================================================================

final case class JFieldFocus[+A <: JAny, +P <: JFocus[JObject]] private[json] (
    value: A,
    name: JString,
    index: Int,
    parent: P
) extends JChildFocus[A, P] {
  val pointer: JPointer = parent.pointer.field(name.value)
}

object JFieldFocus {
  implicit final def toJFieldFocusOps[A <: JAny, P <: JStrongFocus[JObject]](
      in: JFieldFocus[A, P]
  ): JFieldFocusOps[A, P] = new JFieldFocusOps(in)
}

//======================================================================================================================

final case class JItemFocus[+A <: JAny, +P <: JFocus[JArray]] private[json] (
    value: A,
    index: Int,
    parent: P
) extends JChildFocus[A, P] {
  val pointer: JPointer = parent.pointer.item(index)
}

object JItemFocus {
  implicit final def toJItemFocusOps[A <: JAny, P <: JStrongFocus[JArray]](in: JItemFocus[A, P]): JItemFocusOps[A, P] =
    new JItemFocusOps(in)
}
