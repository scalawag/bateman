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

package org.scalawag.bateman.json.focus

import org.scalawag.bateman.json._
import org.scalawag.bateman.json.lens.{JCursorLens, JFocusLens}

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

object JFocus extends JFocusLowPriority {

  object Value {
    def unapply(focus: JFocus[JAny]): Option[JAny] = Some(focus.value)
  }

  implicit class RichJFocus[A <: JAny](me: JFocus[A]) {
    def apply[B <: JAny](op: JFocusLens[A, B]): JResult[JFocus[B]] = op(me)
    def apply[F[+_], B <: JAny](op: JCursorLens[F, A, B]): JResult[JCursor[F, B]] = op(me)

    /** Navigates to a focus via a lens, then decodes the value there. */
    def decodeFrom[B](op: JFocusLens[A, JAny])(implicit dec: JAnyDecoder[B]): JResult[B] =
      op(me).flatMap(_.decode[B])

    /** Navigates to a cursor via a lens, then decodes all values in the cursor. */
    def decodeFrom[B] = new DecodeFromCursor[A, B](me)
  }

  /** Helper class to allow `decodeFrom[B](cursorLens)` to infer `F` from the lens while `B` is explicitly provided. */
  class DecodeFromCursor[A <: JAny, B](me: JFocus[A]) {
    def apply[F[+_]](op: JCursorLens[F, A, JAny])(implicit dec: JAnyDecoder[B], T: cats.Traverse[F]): JResult[F[B]] = {
      import cats.syntax.parallel._
      op(me).flatMap(_.foci.parTraverse(_.decode[B]))
    }
  }

  implicit final def toJFocusJObjectOps[A <: JFocus[JObject]](in: A): JFocusJObjectOps[A] =
    new JFocusJObjectOps(in)

  implicit final def toJFocusJArrayOps[A <: JFocus[JArray]](in: A): JFocusJArrayOps[A] =
    new JFocusJArrayOps(in)
}

/** Low-priority implicits for base ops on JFocus. These apply when the high-priority ops in JFocus companion
  * can't match (e.g., toJFocusOps needs ValueFinder which doesn't exist for abstract JFocus[JAny]).
  */
trait JFocusLowPriority {
  implicit final def toJFocusBaseOps[A <: JAny](in: JFocus[A]): JFocusWeakOps[A] =
    new JFocusWeakOps(in)

}

//======================================================================================================================

final case class JRootFocus[+A <: JAny] private[bateman] (value: A) extends JFocus[A] {
  override val pointer: JPointer = JPointer.Root
  override def parentOption: Option[JFocus[JAny]] = None
  def root: JRootFocus[A] = this
}

object JRootFocus {
  implicit final def toJRootFocusOps[A <: JAny](in: JRootFocus[A]): JRootFocusOps[A] = new JRootFocusOps(in)
}

//======================================================================================================================

sealed trait JChildFocus[+A <: JAny, +P <: JFocus[JAny]] extends JFocus[A] {
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

  def previous: JResult[JFieldFocus[JAny, P]] = parent.field(index - 1)
  def next: JResult[JFieldFocus[JAny, P]] = parent.field(index + 1)
  def first: JFieldFocus[JAny, P] = parent.fields.head
  def last: JFieldFocus[JAny, P] = parent.fields.last
}

object JFieldFocus {
  implicit final def toJFieldFocusOps[A <: JAny, P <: JFocus[JObject]](
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

  def previous: JResult[JItemFocus[JAny, P]] = parent.item(index - 1)
  def next: JResult[JItemFocus[JAny, P]] = parent.item(index + 1)
  def first: JItemFocus[JAny, P] = parent.items.head
  def last: JItemFocus[JAny, P] = parent.items.last
}

object JItemFocus {
  implicit final def toJItemFocusOps[A <: JAny, P <: JFocus[JArray]](in: JItemFocus[A, P]): JItemFocusOps[A, P] =
    new JItemFocusOps(in)
}
