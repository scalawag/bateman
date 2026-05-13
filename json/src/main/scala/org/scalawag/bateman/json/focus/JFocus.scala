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

import cats.syntax.either._
import org.scalawag.bateman.json._
import org.scalawag.bateman.json.JType.Summoner

import scala.annotation.tailrec
import scala.reflect.ClassTag

//======================================================================================================================

/** A focus on a particular JSON value within another JSON value as well as an interpreted aspect of that value.
  *
  * @tparam A the type of the focus' value
  */

sealed trait JFocus[+A <: JAny] {

  /** This focus type with a different value type. For example, on a [[JFieldFocus]][A, P], this is
    * [[JFieldFocus]][X, P]. This allows [[narrow]] and [[asObject]] etc. to return the specific focus subtype.
    */
  type Refocused[+X <: JAny] <: JFocus[X]

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

  /** Returns the root focus of the document containing this focus. */
  def root: JFocus[JAny]

  /** Narrows the value type to a more specific JSON type. Returns the same focus subtype with the narrowed value. */
  def narrow[B <: JAny: ClassTag: Summoner]: JResult[Refocused[B]]

  def asNull: JResult[Refocused[JNull]] = narrow[JNull]
  def asArray: JResult[Refocused[JArray]] = narrow[JArray]
  def asObject: JResult[Refocused[JObject]] = narrow[JObject]
  def asString: JResult[Refocused[JString]] = narrow[JString]
  def asNumber: JResult[Refocused[JNumber]] = narrow[JNumber]
  def asBoolean: JResult[Refocused[JBoolean]] = narrow[JBoolean]

  /** Returns a decoded representation of value in focus. */
  def decode[B](implicit dec: Decoder[A @scala.annotation.unchecked.uncheckedVariance, B]): JResult[B] =
    dec.decode(this)

  def navigate(pointer: JPointer): JResult[JFocus[JAny]] = {
    @tailrec
    def go(todo: List[JPointer.Token], f: JFocus[JAny]): JResult[JFocus[JAny]] =
      todo match {
        case Nil => f.rightNec
        case JPointer.Index(index) :: tail =>
          f.narrow[JArray].flatMap(_.item(index)) match {
            case Right(a) => go(tail, a)
            case left     => left
          }
        case JPointer.Key(key) :: tail =>
          f.narrow[JObject].flatMap(_.field(key)) match {
            case Right(a) => go(tail, a)
            case left     => left
          }
      }

    go(pointer.tokens, this)
  }

  /** Maps the value of this focus without changing the focus structure. */
  def map[B <: JAny](fn: A => B): JFocus[B] = {
    val b = fn(value)
    this match {
      case _: JRootFocus[_]      => JRootFocus(b)
      case ff: JFieldFocus[_, _] => JFieldFocus(b, ff.name, ff.index, ff.parent)
      case ff: JItemFocus[_, _]  => JItemFocus(b, ff.index, ff.parent)
    }
  }

  /** Replicates the exact path of one focus into another root value. */
  private[json] def replicate(root: JAny): JFocus[JAny] = {
    @tailrec
    def getIndices(f: JFocus[_], acc: List[Either[Int, Int]]): List[Either[Int, Int]] =
      f match {
        case _: JRootFocus[_]     => acc
        case x: JFieldFocus[_, _] => getIndices(x.parent, Left(x.index) :: acc)
        case x: JItemFocus[_, _]  => getIndices(x.parent, Right(x.index) :: acc)
      }

    val indices = getIndices(this, Nil)

    @tailrec
    def rebuild(todo: List[Either[Int, Int]], f: JFocus[JAny]): JFocus[JAny] =
      todo match {
        case Nil => f
        case Left(n) :: t =>
          f.narrow[JObject].map(_.fields.lift(n)) match {
            case Right(None) =>
              throw ProgrammerError("object in new document has fewer fields than in the old document!")
            case Right(Some(child)) =>
              rebuild(t, child)
            case _ =>
              throw ProgrammerError(
                s"new document does not have an object where one is expected!\n${f.pointer}\n${f.root.value.render}"
              )
          }
        case Right(n) :: t =>
          f.narrow[JArray].map(_.items.lift(n)) match {
            case Right(None) =>
              throw ProgrammerError("array in new document has fewer items than in the old document!")
            case Right(Some(child)) =>
              rebuild(t, child)
            case _ =>
              throw ProgrammerError("new document does not have an array where one is expected!")
          }
      }

    rebuild(indices, root.asRootFocus)
  }
}

object JFocus {

  object Value {
    def unapply(focus: JFocus[JAny]): Option[JAny] = Some(focus.value)
  }

  implicit final def toJFocusOps[A <: JAny](in: JFocus[A]): JFocusOps[A] = new JFocusOps(in)

  implicit final def toJFocusJObjectOps[A <: JFocus[JObject]](in: A): JFocusJObjectOps[A] =
    new JFocusJObjectOps(in)

  implicit final def toJFocusJArrayOps[A <: JFocus[JArray]](in: A): JFocusJArrayOps[A] =
    new JFocusJArrayOps(in)

}

//======================================================================================================================

final case class JRootFocus[+A <: JAny] private[bateman] (value: A) extends JFocus[A] {
  type Refocused[+X <: JAny] = JRootFocus[X]
  override val pointer: JPointer = JPointer.Root
  override def parentOption: Option[JFocus[JAny]] = None
  override def root: JRootFocus[A] = this

  override def narrow[B <: JAny: ClassTag: Summoner]: JResult[JRootFocus[B]] =
    value match {
      case b: B => JRootFocus(b).rightNec
      case _    => JsonTypeMismatch(this, JType[B]).leftNec
    }
}

object JRootFocus {
  implicit final def toJRootFocusOps[A <: JAny](in: JRootFocus[A]): JRootFocusOps[A] = new JRootFocusOps(in)
}

//======================================================================================================================

sealed trait JChildFocus[+A <: JAny, +P <: JFocus[JAny]] extends JFocus[A] {
  val parent: P
  override def parentOption: Option[JFocus[JAny]] = Some(parent)
  override def root: JFocus[JAny] = parent.root
}

//======================================================================================================================

final case class JFieldFocus[+A <: JAny, +P <: JFocus[JObject]] private[json] (
    value: A,
    name: JString,
    index: Int,
    parent: P
) extends JChildFocus[A, P] {
  type Refocused[+X <: JAny] = JFieldFocus[X, P @scala.annotation.unchecked.uncheckedVariance]
  val pointer: JPointer = parent.pointer.field(name.value)

  override def narrow[B <: JAny: ClassTag: Summoner]: JResult[JFieldFocus[B, P]] =
    value match {
      case b: B => JFieldFocus(b, name, index, parent).rightNec
      case _    => JsonTypeMismatch(this, JType[B]).leftNec
    }

  def previous: JResult[JFieldFocus[JAny, P]] = parent.field(index - 1)
  def next: JResult[JFieldFocus[JAny, P]] = parent.field(index + 1)
  def first: JFieldFocus[JAny, P] = parent.fields.head
  def last: JFieldFocus[JAny, P] = parent.fields.last
}

object JFieldFocus {
  implicit final def toJFieldFocusOps[A <: JAny, P <: JFocus[JObject]](in: JFieldFocus[A, P]): JFieldFocusOps[A, P] =
    new JFieldFocusOps(in)
}

//======================================================================================================================

final case class JItemFocus[+A <: JAny, +P <: JFocus[JArray]] private[json] (
    value: A,
    index: Int,
    parent: P
) extends JChildFocus[A, P] {
  type Refocused[+X <: JAny] = JItemFocus[X, P @scala.annotation.unchecked.uncheckedVariance]
  val pointer: JPointer = parent.pointer.item(index)

  override def narrow[B <: JAny: ClassTag: Summoner]: JResult[JItemFocus[B, P]] =
    value match {
      case b: B => JItemFocus(b, index, parent).rightNec
      case _    => JsonTypeMismatch(this, JType[B]).leftNec
    }

  def previous: JResult[JItemFocus[JAny, P]] = parent.item(index - 1)
  def next: JResult[JItemFocus[JAny, P]] = parent.item(index + 1)
  def first: JItemFocus[JAny, P] = parent.items.head
  def last: JItemFocus[JAny, P] = parent.items.last
}

object JItemFocus {
  implicit final def toJItemFocusOps[A <: JAny, P <: JFocus[JArray]](in: JItemFocus[A, P]): JItemFocusOps[A, P] =
    new JItemFocusOps(in)
}
