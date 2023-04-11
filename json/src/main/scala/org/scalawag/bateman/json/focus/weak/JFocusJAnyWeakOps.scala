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

package org.scalawag.bateman.json.focus.weak

import cats.syntax.either._
import org.scalawag.bateman.json.JType.Summoner
import org.scalawag.bateman.json._
import org.scalawag.bateman.json.lens._
import org.scalawag.bateman.json.focus._
import org.scalawag.bateman.json.focus.weak._

import scala.annotation.tailrec
import scala.reflect.ClassTag

class JFocusJAnyWeakOps[A <: JAny](me: JFocus[A]) {
  def narrow[B <: JAny: ClassTag: Summoner]: JResult[JFocus[B]] =
    me.value match {
      case b: B => {
          me match {
            case _: JRootFocus[_]     => JRootFocus(b)
            case f: JFieldFocus[_, _] => JFieldFocus(b, f.name, f.index, f.parent)
            case f: JItemFocus[_, _]  => JItemFocus(b, f.index, f.parent)
          }
        }.rightNec
      case _ => JsonTypeMismatch(me, JType[B]).leftNec
    }

  def asNull: JResult[JFocus[JNull]] = narrow[JNull]
  def asArray: JResult[JFocus[JArray]] = narrow[JArray]
  def asObject: JResult[JFocus[JObject]] = narrow[JObject]
  def asString: JResult[JFocus[JString]] = narrow[JString]
  def asNumber: JResult[JFocus[JNumber]] = narrow[JNumber]
  def asBoolean: JResult[JFocus[JBoolean]] = narrow[JBoolean]

  def parent: JResult[JFocus[JAny]] =
    me match {
      case f: JRootFocus[_]     => NoParent(f).leftNec
      case f: JChildFocus[_, _] => f.parent.rightNec
    }

  /** The root JSON value of this focus.
    *
    * Note that this is not defined by the structure of the JSON values (which do not contain upward references and
    * therefore do not reference their own root, directly or indirectly) but merely the value which was the root
    * when the focus was created (usually through [[JAny.asRootFocus]]).
    */

  def root: JFocus[JAny] =
    me match {
      case f: JRootFocus[_]     => f
      case f: JChildFocus[_, _] => f.parent.root
    }

  /** Applies a modification to the current focus value and returns a new focus into the modified document
    * at the modified value. Returns a failure if the modification function returns a failure.
    */
  def modifyF[B <: JAny](fn: JFocus[A] => JResult[B]): JResult[JFocus[B]] = fn(me).map(replace)

  /** Applies a modification to the current focus value and returns a new focus into the modified document
    * at the modified value.
    */
  def modify[B <: JAny](fn: JFocus[A] => B): JFocus[B] = replace(fn(me))

  /** Applies a modification to the value in focus and returns a new focus into the modified document
    * at the modified value. Returns a failure if the modification function returns a failure.
    */
  def modifyValueF[B <: JAny](fn: A => JResult[B]): JResult[JFocus[B]] = fn(me.value).map(replace)

  /** Applies a modification to the value in focus and returns a new focus into the modified document
    * at the modified value.
    */
  def modifyValue[B <: JAny](fn: A => B): JFocus[B] = replace(fn(me.value))

  /** Returns a focus into a new JSON document which is a copy of the source focus' document except that the
    * value in focus has been replaced by an encoded version of the input.
    */
//    def encode[B: JAnyEncoder](b: B): JFocus[JAny] = replace(b.toJAny)

  /** Returns a focus into a new JSON document which is a copy of the source focus' document except that the
    * value in focus has been replaced by the specified value.
    */
  def replace[B <: JAny](newValue: B): JFocus[B] = {
    // Rebuild the JSON document from the focus up to the root. The focus parent is not used other than for its
    // path information. The related JAnys become obsolete as soon as we start rebuilding the document.
    @tailrec
    def rebuildValues(value: JAny, focus: JFocus[JAny]): JAny =
      focus match {
        case f: JRootFocus[_] =>
          // We're at the root, just replace it with the specified value.
          value

        case f: JItemFocus[_, _] =>
          // We're inside an array. Replace the the item at the specified index with the new value specified.
          rebuildValues(f.parent.value.updated(f.index, value), f.parent)

        case f: JFieldFocus[_, _] =>
          // We're inside an object. Replace the the item at the specified index with the new value specified.
          rebuildValues(f.parent.value.updated(f.index, value), f.parent)
      }

    me.replicate(rebuildValues(newValue.stripLocation, me)).asInstanceOf[JFocus[B]]
  }

  /** Returns a copy of the document with the value in focus deleted. The new focus is the parent of the
    * original focus.
    */
  def delete(): JResult[JFocus[JAny]] =
    me match {
      case f: JRootFocus[_]     => NoParent(f).leftNec
      case f: JItemFocus[_, _]  => f.parent.modifyValue(_.delete(f.index)).rightNec
      case f: JFieldFocus[_, _] => f.parent.modifyValue(_.delete(f.index)).rightNec
    }

  /** Returns a decoded representation of value in focus. */
  def decode[B](implicit dec: Decoder[A, B]): JResult[B] = dec.decode(me)

  /** Replicates the exact path of one focus into another root value. This function assumes that the structure
    * of both documents is the same, at least with respect to source focus' path. Note that the path uses indices
    * for navigating down into both arrays and objects. This is to make it possible to preserve foci into duplicate
    * fields. When these assumptions are not met, this function throws a ProgrammerError. This is not meant as a
    * general-purpose tool. It's only used internally on very well-defined inputs.
    */
  def replicate(root: JAny): JFocus[JAny] = {
    @tailrec
    def getIndices(f: JFocus[_], acc: List[Either[Int, Int]]): List[Either[Int, Int]] =
      f match {
        case _: JRootFocus[_]     => acc
        case x: JFieldFocus[_, _] => getIndices(x.parent, Left(x.index) :: acc)
        case x: JItemFocus[_, _]  => getIndices(x.parent, Right(x.index) :: acc)
      }

    // Determine the indices of the nodes in our value's ancestry.
    val indices = getIndices(me, Nil)

    // Now, follow that same path from the new root.
    @tailrec
    def rebuild(todo: List[Either[Int, Int]], f: JFocus[JAny]): JFocus[JAny] =
      todo match {
        case Nil => f
        case Left(n) :: t =>
          f.asObject.map(_.fields.lift(n)) match {
            case Right(None) =>
              throw ProgrammerError("object in new document has fewer fields than in the old document!")
            case Right(Some(child)) =>
              rebuild(t, child)
            case ee =>
              throw ProgrammerError(
                s"new document does not have an object where one is expected!\n${f.pointer}\n${f.root.value.render}"
              )
          }
        case Right(n) :: t =>
          f.asArray.map(_.items.lift(n)) match {
            case Right(None) =>
              throw ProgrammerError("array in new document has fewer items than in the old document!")
            case Right(Some(child)) =>
              rebuild(t, child)
            case ee =>
              throw ProgrammerError("new document does not have an array where one is expected!")
          }
      }

    rebuild(indices, root.asRootFocus)
  }

  // This is only used in a test generator now. TODO: drop it? or make it accessible to all, but with failures.
  def navigate(pointer: JPointer): JResult[JFocus[JAny]] = {
    @tailrec
    def go(todo: List[JPointer.Token], focus: JFocus[JAny]): JResult[JFocus[JAny]] =
      todo match {
        case Nil => focus.rightNec
        case JPointer.Index(index) :: tail =>
          focus.asArray.flatMap(_.item(index)) match {
            case Right(a) => go(tail, a)
            case left     => left
          }
        case JPointer.Key(key) :: tail =>
          // TODO: error handling here... if the structure is wonky
          focus.asObject.flatMap(_.field(key)) match {
            case Right(a) => go(tail, a)
            case left     => left
          }
      }

    go(pointer.tokens, me)
  }

  private def getCreatableLensFieldNames(idJLens: CreatableJLens[_, _]): List[String] =
    idJLens match {
      case CompositeCreatableJLens(l, r) => getCreatableLensFieldNames(l) ::: getCreatableLensFieldNames(r)
      case FieldJLens(name)              => List(name)
      case FocusJLens()                  => Nil
      case _: NarrowJLens[_]             => Nil
    }

  /** Writes a value to the JSON structure through the specified lens, overwriting any existing values.
    * As such, this function is guaranteed not to fail. Modifies the document, but leaves the focus in
    * the corresponding location of the new document.
    */

  def overwriteTo[B <: JAny, C <: JAny](
      lens: CreatableJLens[B, C],
      value: JAny,
      prepend: Boolean = false
  ): JFocus[JAny] = {
    @tailrec
    def go(todo: List[String], acc: JFocus[JAny]): JFocus[JAny] =
      todo match {
        case Nil    => acc
        case h :: t =>
          // If the focus is an object, reuse it. If it is not, overwrite the value with an empty object.
          val oacc =
            acc match {
              case JFocus.Value(o: JObject) => acc.as(o)
              case _                        => acc.replace(JObject.Empty)
            }

          oacc.fieldOption(h) match {
            case Right(Some(fieldValue)) =>
              // It exists, just move the focus to it.
              go(t, fieldValue)

            case Right(None) =>
              // It does not exist. Create it and move the focus to it.
              go(
                t,
                acc.asObject
                  .map { f =>
                    if (prepend)
                      f.prepend(h, JObject()).fields.head
                    else
                      f.append(h, JObject()).fields.last
                  }
                  // It's safe to do a .getOrElse(???) here because we just added the field, so we know it's there.
                  .getOrElse(???)
              )
          }
      }

    val newRoot = go(getCreatableLensFieldNames(lens), me).replace(value).root.value
    me.replicate(newRoot)
  }

  /** Writes a value to the JSON structure through the specified lens. Fails if any of the values
    * along the path already exist and are not objects.
    */
  def writeTo[B <: JAny, C <: JAny](
      lens: CreatableJLens[B, C],
      value: JAny,
      prepend: Boolean = false
  ): JResult[JFocus[JAny]] = {
    @tailrec
    def go(todo: List[String], acc: JFocus[JAny]): JResult[JFocus[JAny]] =
      todo match {
        case Nil => acc.rightNec
        case h :: t =>
          acc.asObject.flatMap(_.fieldOption(h)) match {
            case Right(Some(fieldValue)) =>
              // It exists, just move the focus to it.
              go(t, fieldValue)

            case Right(None) =>
              // It does not exist. Create it and move the focus to it.
              // It's safe to do a .getOrElse(???) here because we just added the field, so we know it's there.
              val newAccResult = acc.asObject.map { f =>
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

    go(getCreatableLensFieldNames(lens), me).map(_.replace(value))
  }

  def map[B <: JAny](fn: A => B): JFocus[B] =
    me match {
      case f: JRootFocus[A]     => JRootFocus(fn(f.value))
      case f: JFieldFocus[A, _] => JFieldFocus(fn(f.value), f.name, f.index, f.parent)
      case f: JItemFocus[A, _]  => JItemFocus(fn(f.value), f.index, f.parent)
    }

  def as[B <: JAny](value: B): JFocus[B] = me.map(_ => value)
}
