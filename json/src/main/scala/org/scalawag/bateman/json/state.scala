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

package org.scalawag.bateman.json

import cats.Traverse
import cats.data.{IndexedStateT, StateT}
import cats.syntax.either._
import cats.syntax.traverse._
import org.scalawag.bateman.json.lens._
import org.scalawag.bateman.json.syntax._
import scala.reflect.ClassTag
import org.scalawag.bateman.json.JType.Summoner
import org.scalawag.bateman.json.focus.JFocus

/** Provides a monadic state API for navigating and editing JSON documents using [[JFocus]] as the state.
  *
  * Operations compose in for-comprehensions, threading the current focus through a sequence of navigation,
  * modification, and decoding steps.
  *
  * {{{
  * import org.scalawag.bateman.json.state._
  *
  * val edit = for {
  *   _ <- down("users")
  *   _ <- down(0)
  *   _ <- modify(_ => JString("replaced"))
  * } yield ()
  *
  * edit.runS(json).shouldSucceed
  * }}}
  */
package object state {

  /** A state monad over a [[JFocus]], producing a value of type `B`. The focus type does not change. */
  type State[A <: JAny, B] = StateT[JResult, JFocus[A], B]

  /** An indexed state monad over a [[JFocus]] where the focus value type changes from `A` to `B`. */
  type IndexedState[A <: JAny, B <: JAny] = IndexedStateT[JResult, JFocus[A], JFocus[B], B]

  /** Creates an [[IndexedState]] from a function that transforms one focus into another. */
  def apply[A <: JAny, B <: JAny](fn: JFocus[A] => JResult[JFocus[B]]): IndexedState[A, B] =
    IndexedStateT.apply[JResult, JFocus[A], JFocus[B], B] { sin =>
      fn(sin).map(sout => sout -> sout.value)
    }

  /** Returns the current focus as the output value. */
  def focus[A <: JAny]: State[A, JFocus[A]] = StateT.get[JResult, JFocus[A]]

  /** Returns the JSON value at the current focus. */
  def value[A <: JAny]: State[A, A] = focus.map(_.value)

  /** Moves the focus to the root of the document. */
  def root[A <: JAny]: IndexedState[A, JAny] = apply((f: JFocus[A]) => f.root.asRight)

  /** Navigates down into the document using an [[IdJLens]]. */
  def down[A <: JAny, B <: JAny](lens: IdJLens[A, B]): IndexedState[A, B] = apply(lens(_))

  /** Navigates down into the document using a [[CreatableJLens]]. */
  def down[A <: JAny, B <: JAny](lens: CreatableJLens[A, B]): IndexedState[A, B] = apply(lens(_))

  /** Narrows the focus to a specific JSON type. Fails if the value is not of the target type. */
  def narrow[B <: JAny: ClassTag: Summoner]: IndexedState[JAny, B] =
    apply((f: JFocus[JAny]) => f.narrow[B])

  /** Moves the focus up to the parent of the current focus. Fails if the focus is at the root. */
  def up[A <: JAny]: IndexedState[A, JAny] = apply((f: JFocus[A]) =>
    f.parentOption match {
      case Some(p) => p.rightNec
      case None    => NoParent(f).leftNec
    }
  )

  /** Transforms the value at the current focus using a pure function on the value.
    * The result is encoded to JSON via the implicit [[Encoder]].
    */
  def modify[A <: JAny, B, C <: JAny](fn: A => B)(implicit encoder: Encoder[B, C]): IndexedState[A, C] =
    IndexedStateT.apply[JResult, JFocus[A], JFocus[C], C] { fa =>
      val fb = fa.replace(fn(fa.value).toJAny)
      (fb -> fb.value).rightNec
    }

  /** Transforms the value at the current focus using a fallible function on the value.
    * The result is encoded to JSON via the implicit [[Encoder]].
    */
  def modifyF[A <: JAny, B, C <: JAny](fn: A => JResult[B])(implicit encoder: Encoder[B, C]): IndexedState[A, C] =
    IndexedStateT.apply[JResult, JFocus[A], JFocus[C], C] { fa =>
      fn(fa.value).map(b => fa.replace(b.toJAny)).map(fb => fb -> fb.value)
    }

  /** Transforms the value at the current focus using a pure function on the focus.
    * The result is encoded to JSON via the implicit [[Encoder]].
    */
  def modifyFocus[A <: JAny, B, C <: JAny](fn: JFocus[A] => B)(implicit encoder: Encoder[B, C]): IndexedState[A, C] =
    IndexedStateT.apply[JResult, JFocus[A], JFocus[C], C] { fa =>
      val fb = fa.replace(fn(fa).toJAny)
      (fb -> fb.value).rightNec
    }

  /** Transforms the value at the current focus using a fallible function on the focus.
    * The result is encoded to JSON via the implicit [[Encoder]].
    */
  def modifyFocusF[A <: JAny, B, C <: JAny](
      fn: JFocus[A] => JResult[B]
  )(implicit encoder: Encoder[B, C]): IndexedState[A, C] =
    IndexedStateT.apply[JResult, JFocus[A], JFocus[C], C] { fa =>
      fn(fa).map(b => fa.replace(b.toJAny)).map(fb => fb -> fb.value)
    }

  /** Replaces the value at the current focus with the given value, encoding it via the implicit [[Encoder]]. */
  def replace[A <: JAny, B, C <: JAny](value: B)(implicit encoder: Encoder[B, C]): IndexedState[A, C] =
    IndexedStateT.apply[JResult, JFocus[A], JFocus[C], C] { sin =>
      val sout = sin.replace(value)
      (sout -> sout.value).rightNec
    }

  /** Deletes the value at the current focus and moves to the parent. Fails if the focus is at the root. */
  def delete[A <: JAny](): IndexedState[A, JAny] = apply((f: JFocus[A]) => f.delete())

  /** Encodes a value and replaces the current focus with the encoded JSON. */
  def encode[A <: JAny, B](a: B)(implicit JAnyEncoder: JAnyEncoder[B]): IndexedState[A, JAny] = replace(a.toJAny)

  /** Encodes a value and writes it to the location specified by the lens, creating intermediate
    * structure as needed. The focus remains at its current location in the updated document.
    */
  def encodeTo[A <: JAny, B <: JAny, C](
      lens: CreatableJLens[A, B],
      value: C
  )(implicit encoder: Encoder[C, B]): State[A, B] =
    StateT.apply[JResult, JFocus[A], B] { fa =>
      fa.encodeTo(lens, value).map { fb =>
        // reroot this focus into the new document
        val fa2 = fa.replicate(fb.root.value).asInstanceOf[JFocus[A]]
        fa2 -> fb.value
      }
    }

  /** Optionally encodes a value and writes it to the location specified by the lens. If the value
    * is [[None]], the document is left unchanged.
    */
  def encodeTo[A <: JAny, B <: JAny, C](
      lens: CreatableJLens[A, B],
      valueOpt: Option[C]
  )(implicit encoder: Encoder[C, B]): State[A, Option[B]] =
    valueOpt match {
      case Some(value) => encodeTo(lens, value).map(Some(_))
      case None        => StateT.pure(None)
    }

  /** Decodes the value at the current focus into a Scala type. */
  def decode[A](implicit Decoder: Decoder[JAny, A]): State[JAny, A] =
    StateT.inspectF[JResult, JFocus[JAny], A](_.decode[A])

  /** Navigates to a location specified by a lens and decodes the value found there.
    * The focus remains at its current location.
    */
  def decodeThrough[B] = new DecodeThroughState[B]

  class DecodeThroughState[B] {

    /** Decodes a single value at the lens target. */
    def apply[A <: JAny](lens: JFocusLens[JAny, A])(implicit decoder: Decoder[A, B]): State[JAny, B] =
      StateT.inspectF[JResult, JFocus[JAny], B] { in =>
        lens(in).flatMap(_.decode[B])
      }

    /** Decodes multiple values at the cursor lens targets. */
    def apply[F[+_]: Traverse, A <: JAny](
        lens: JCursorLens[F, JAny, A]
    )(implicit decoder: Decoder[A, B]): State[JAny, F[B]] =
      StateT.inspectF[JResult, JFocus[JAny], F[B]] { in =>
        lens(in).flatMap(_.foci.traverse(_.decode[B]))
      }
  }
}
