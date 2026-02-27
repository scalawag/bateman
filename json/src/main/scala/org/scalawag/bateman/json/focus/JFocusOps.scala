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
import org.scalawag.bateman.json.lens._

import scala.annotation.tailrec

/** Provides document-modification and lens-application operations on any [[JFocus]].
  *
  * These are defined as an implicit class (rather than directly on the sealed trait) so that the
  * more specific ops classes ([[JRootFocusOps]], [[JFieldFocusOps]], [[JItemFocusOps]]) take
  * priority when the concrete focus subtype is known. Those ops classes return the specific
  * subtype (e.g., `JFieldFocus[O, P]`) while these return the weaker, abstract `JFocus[C]`.
  */
class JFocusOps[A <: JAny](me: JFocus[A]) {

  /** Applies a pure value transformation to the current focus and returns a new focus into the modified document. */
  def modify[B <: JAny](fn: A => B): JFocus[B] = replace(fn(me.value))

  /** Applies a fallible value transformation to the current focus and returns a new focus into the modified document. */
  def modify[B <: JAny](fn: A => JResult[B])(implicit d: DummyImplicit): JResult[JFocus[B]] =
    fn(me.value).map(replace(_))

  /** Applies a pure focus transformation to the current focus and returns a new focus into the modified document. */
  def modifyFocus[B <: JAny](fn: JFocus[A] => B): JFocus[B] =
    replace(fn(me))

  /** Applies a fallible focus transformation to the current focus and returns a new focus into the modified document. */
  def modifyFocus[B <: JAny](fn: JFocus[A] => JResult[B])(implicit d: DummyImplicit): JResult[JFocus[B]] =
    fn(me).map(replace(_))

  /** Returns a focus into a new JSON document which is a copy of the source focus' document except that the
    * value in focus has been replaced by the specified value (or its encoded representation).
    */
  def replace[B, C <: JAny](newValue: B)(implicit enc: Encoder[B, C]): JFocus[C] = {
    val encoded = enc.encode(newValue)
    val stripped = LocationStripper.strip(encoded)
    @tailrec
    def rebuildValues(value: JAny, f: JFocus[JAny]): JAny =
      f match {
        case _: JRootFocus[_] =>
          value
        case ff: JItemFocus[_, _] =>
          rebuildValues(ff.parent.value.updated(ff.index, value), ff.parent)
        case ff: JFieldFocus[_, _] =>
          rebuildValues(ff.parent.value.updated(ff.index, value), ff.parent)
      }

    me.replicate(rebuildValues(stripped, me)).as(stripped)
  }

  /** Returns a copy of the document with the value in focus deleted. The new focus is the parent of the
    * original focus.
    */
  def delete(): JResult[JFocus[JAny]] =
    me match {
      case f: JRootFocus[_] => NoParent(f).leftNec
      case f: JItemFocus[_, _] =>
        new JFocusOps(f.parent).replace(f.parent.value.delete(f.index)).rightNec
      case f: JFieldFocus[_, _] =>
        new JFocusOps(f.parent).replace(f.parent.value.delete(f.index)).rightNec
    }

  private def getCreatableLensFieldNames(idJLens: CreatableJLens[_, _]): List[String] =
    idJLens match {
      case CompositeCreatableJLens(l, r) => getCreatableLensFieldNames(l) ::: getCreatableLensFieldNames(r)
      case FieldJLens(name)              => List(name)
      case FocusJLens()                  => Nil
      case _: NarrowJLens[_]             => Nil
    }

  def overwriteTo[B <: JAny, C <: JAny, D: JAnyEncoder](
      lens: CreatableJLens[B, C],
      value: D,
      prepend: Boolean = false
  ): JFocus[JAny] = {
    @tailrec
    def go(todo: List[String], acc: JFocus[JAny]): JFocus[JAny] =
      todo match {
        case Nil    => acc
        case h :: t =>
          val oacc: JFocus[JObject] =
            acc match {
              case JFocus.Value(o: JObject) => acc.map((_: JAny) => o)
              case _                        => new JFocusOps(acc).replace(JObject.Empty)
            }

          oacc.fieldOption(h) match {
            case Right(Some(fieldValue)) =>
              go(t, fieldValue)

            case Right(None) =>
              go(
                t,
                (acc.narrow[JObject]: JResult[JFocus[JObject]])
                  .map { f =>
                    if (prepend)
                      f.prepend(h, JObject()).fields.head
                    else
                      f.append(h, JObject()).fields.last
                  }
                  .getOrElse(???)
              )

            case _ => ???
          }
      }

    import syntax._
    val target = go(getCreatableLensFieldNames(lens), me)
    val newRoot = new JFocusOps(target).replace(value.toJAny).root.value
    me.replicate(newRoot)
  }

  def writeTo[B <: JAny, C <: JAny, D, E <: JAny](
      lens: CreatableJLens[B, C],
      value: D,
      prepend: Boolean = false
  )(implicit enc: Encoder[D, E]): JResult[JFocus[E]] = {
    @tailrec
    def go(todo: List[String], acc: JFocus[JAny]): JResult[JFocus[JAny]] =
      todo match {
        case Nil => acc.rightNec
        case h :: t =>
          (acc.narrow[JObject]: JResult[JFocus[JObject]]).flatMap(_.fieldOption(h)) match {
            case Right(Some(fieldValue)) =>
              go(t, fieldValue)

            case Right(None) =>
              val newAccResult = (acc.narrow[JObject]: JResult[JFocus[JObject]]).map { f =>
                if (prepend)
                  f.prepend(h, JObject()).fields.head
                else
                  f.append(h, JObject()).fields.last
              }

              newAccResult match {
                case Right(newAcc) => go(t, newAcc)
                case Left(ee)      => ee.asLeft
              }

            case Left(ee) =>
              ee.asLeft
          }
      }

    go(getCreatableLensFieldNames(lens), me).map(t => new JFocusOps(t).replace(value))
  }

  def apply[B <: JAny](op: JFocusLens[A, B]): JResult[JFocus[B]] = op(me)
  def apply[F[+_], B <: JAny](op: JCursorLens[F, A, B]): JResult[JCursor[F, B]] = op(me)

  /** Navigates to a focus via a lens, then decodes the value there. */
  def decodeFrom[B](op: JFocusLens[A, JAny])(implicit dec: JAnyDecoder[B]): JResult[B] =
    op(me).flatMap(_.decode[B])

  /** Navigates to a cursor via a lens, then decodes all values in the cursor. */
  def decodeFrom[B] = new JFocusOps.DecodeFromCursor[A, B](me)
}

object JFocusOps {
  /** Helper class to allow `decodeFrom[B](cursorLens)` to infer `F` from the lens while `B` is explicitly provided. */
  class DecodeFromCursor[A <: JAny, B](me: JFocus[A]) {
    def apply[F[+_]](op: JCursorLens[F, A, JAny])(implicit dec: JAnyDecoder[B], T: cats.Traverse[F]): JResult[F[B]] = {
      import cats.syntax.parallel._
      op(me).flatMap(_.foci.parTraverse(_.decode[B]))
    }
  }
}
