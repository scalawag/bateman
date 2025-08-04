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
import org.scalawag.bateman.json.JType.Summoner
import org.scalawag.bateman.json._
import org.scalawag.bateman.json.lens._

import scala.annotation.tailrec
import scala.reflect.ClassTag

/** Extends [[JFocus]] with methods that can be used when the JSON value in focus is weak (it's parentage is not
  * known) and the value is a [[JAny]]. The operations return weak foci (or something other than a focus).
  */

class JFocusWeakOps[A <: JAny](me: JFocus[A]) {
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

  def root: JFocus[JAny] =
    me match {
      case f: JRootFocus[_]     => f
      case f: JChildFocus[_, _] => f.parent.root
    }

  def parent: JResult[JFocus[JAny]] =
    me match {
      case f: JRootFocus[_]     => NoParent(f).leftNec
      case f: JChildFocus[_, _] => f.parent.rightNec
    }

  /** Applies a modification to the current focus value and returns a new focus into the modified document
    * at the modified value. The function argument can be either a pure or fallible transformation of the
    * focus or its value, resolved via the [[JFocusWeakOps.ModifyMagnet]] magnet pattern.
    */
  def modify(magnet: JFocusWeakOps.ModifyMagnet[A]): magnet.Out = magnet(me)

  /** Returns a focus into a new JSON document which is a copy of the source focus' document except that the
    * value in focus has been replaced by the specified value (or its encoded representation).
    */
  def replace[B, C <: JAny](newValue: B)(implicit enc: Encoder[B, C]): JFocus[C] = {
    val encoded = enc.encode(newValue)
    val stripped = LocationStripper.forJAny.stripLocation(encoded).asInstanceOf[C]
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

    replicateAs(rebuildValues(stripped, me), stripped)
  }

  /** Returns a copy of the document with the value in focus deleted. The new focus is the parent of the
    * original focus.
    */
  def delete(): JResult[JFocus[JAny]] =
    me match {
      case f: JRootFocus[_]     => NoParent(f).leftNec
      case f: JItemFocus[_, _]  => new JFocusWeakOps(f.parent).replace(f.parent.value.delete(f.index)).rightNec
      case f: JFieldFocus[_, _] => new JFocusWeakOps(f.parent).replace(f.parent.value.delete(f.index)).rightNec
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
            case _ =>
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

  /** Like [[replicate]], but uses the provided typed leaf value at the focus position instead of whatever
    * is in the document. This allows constructing a typed `JFocus[C]` without casting.
    */
  private[json] def replicateAs[C <: JAny](root: JAny, leafValue: C): JFocus[C] = {
    @tailrec
    def getIndices(f: JFocus[_], acc: List[Either[Int, Int]]): List[Either[Int, Int]] =
      f match {
        case _: JRootFocus[_]     => acc
        case x: JFieldFocus[_, _] => getIndices(x.parent, Left(x.index) :: acc)
        case x: JItemFocus[_, _]  => getIndices(x.parent, Right(x.index) :: acc)
      }

    val indices = getIndices(me, Nil)

    @tailrec
    def rebuild(todo: List[Either[Int, Int]], f: JFocus[JAny]): JFocus[C] =
      todo match {
        case Nil =>
          // At the leaf position, construct the focus with our typed value.
          f match {
            case _: JRootFocus[_]      => JRootFocus(leafValue)
            case ff: JFieldFocus[_, _] => JFieldFocus(leafValue, ff.name, ff.index, ff.parent)
            case ff: JItemFocus[_, _]  => JItemFocus(leafValue, ff.index, ff.parent)
          }
        case Left(n) :: t =>
          f.asObject.map(_.fields.lift(n)) match {
            case Right(Some(child)) => rebuild(t, child)
            case Right(None) =>
              throw ProgrammerError("object in new document has fewer fields than in the old document!")
            case _ =>
              throw ProgrammerError(
                s"new document does not have an object where one is expected!\n${f.pointer}\n${f.root.value.render}"
              )
          }
        case Right(n) :: t =>
          f.asArray.map(_.items.lift(n)) match {
            case Right(Some(child)) => rebuild(t, child)
            case Right(None) =>
              throw ProgrammerError("array in new document has fewer items than in the old document!")
            case _ =>
              throw ProgrammerError("new document does not have an array where one is expected!")
          }
      }

    rebuild(indices, root.asRootFocus)
  }

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
          // If the focus is an object, reuse it. If it is not, overwrite the value with an empty object.
          val oacc =
            acc match {
              case JFocus.Value(o: JObject) => acc.as(o)
              case _                        => new JFocusWeakOps(acc).replace(JObject.Empty)
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

            case _ => ??? // This should not happen because we got here by the presence of this field name!
          }
      }

    import syntax._
    val target = go(getCreatableLensFieldNames(lens), me)
    val newRoot = new JFocusWeakOps(target).replace(value.toJAny).root.value
    me.replicate(newRoot)
  }

  /** Writes a value to the JSON structure through the specified lens. Fails if any of the values
    * along the path already exist and are not objects.
    */
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

    go(getCreatableLensFieldNames(lens), me).map(t => new JFocusWeakOps(t).replace(value))
  }

  def map[B <: JAny](fn: A => B): JFocus[B] =
    me match {
      case f: JRootFocus[A]     => JRootFocus(fn(f.value))
      case f: JFieldFocus[A, _] => JFieldFocus(fn(f.value), f.name, f.index, f.parent)
      case f: JItemFocus[A, _]  => JItemFocus(fn(f.value), f.index, f.parent)
    }

  def as[B <: JAny](value: B): JFocus[B] = me.map(_ => value)
}

object JFocusWeakOps {
  trait ModifyMagnet[A <: JAny] {
    type Out
    def apply(focus: JFocus[A]): Out
  }

  object ModifyMagnet extends JFocusModifyMagnetLowPriority {
    implicit def focusFallible[A <: JAny, B <: JAny](
        fn: JFocus[A] => JResult[B]
    ): ModifyMagnet[A] { type Out = JResult[JFocus[B]] } =
      new ModifyMagnet[A] {
        type Out = JResult[JFocus[B]]
        def apply(focus: JFocus[A]): JResult[JFocus[B]] = fn(focus).map(new JFocusWeakOps(focus).replace(_))
      }

    implicit def valueFallible[A <: JAny, B <: JAny](
        fn: A => JResult[B]
    ): ModifyMagnet[A] { type Out = JResult[JFocus[B]] } =
      new ModifyMagnet[A] {
        type Out = JResult[JFocus[B]]
        def apply(focus: JFocus[A]): JResult[JFocus[B]] = fn(focus.value).map(new JFocusWeakOps(focus).replace(_))
      }
  }

  trait JFocusModifyMagnetLowPriority {
    implicit def focusPure[A <: JAny, B <: JAny](fn: JFocus[A] => B): ModifyMagnet[A] { type Out = JFocus[B] } =
      new ModifyMagnet[A] {
        type Out = JFocus[B]
        def apply(focus: JFocus[A]): JFocus[B] = new JFocusWeakOps(focus).replace(fn(focus))
      }

    implicit def valuePure[A <: JAny, B <: JAny](fn: A => B): ModifyMagnet[A] { type Out = JFocus[B] } =
      new ModifyMagnet[A] {
        type Out = JFocus[B]
        def apply(focus: JFocus[A]): JFocus[B] = new JFocusWeakOps(focus).replace(fn(focus.value))
      }
  }
}
