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

package org.scalawag.bateman.json

import cats.Traverse
import cats.data.{IndexedStateT, StateT}
import cats.syntax.either._
import cats.syntax.traverse._
import org.scalawag.bateman.json.lens._
import org.scalawag.bateman.json.syntax._
import org.scalawag.bateman.json.focus.weak._
import org.scalawag.bateman.json.focus.{JFocus, Single}

// Generally return the focus aspect.

package object state {
  type State[A <: JAny, B] = StateT[JResult, JFocus[A], B]
  type IndexedState[A <: JAny, B <: JAny] = IndexedStateT[JResult, JFocus[A], JFocus[B], B]

  def apply[A <: JAny, B <: JAny](fn: JFocus[A] => JResult[JFocus[B]]): IndexedState[A, B] =
    IndexedStateT.apply[JResult, JFocus[A], JFocus[B], B] { sin =>
      fn(sin).map(sout => sout -> sout.value)
    }

  def apply[A <: JAny, B <: JAny](lens: JLens[Single, A, B]): IndexedState[A, B] =
    IndexedStateT.apply[JResult, JFocus[A], JFocus[B], B] { sin =>
      lens(sin).map(sout => sout.foci -> sout.foci.value)
    }

  def focus[A <: JAny]: State[A, JFocus[A]] = StateT.get[JResult, JFocus[A]]
  def value[A <: JAny]: State[A, A] = focus.map(_.value)
  def root[A <: JAny]: IndexedState[A, JAny] = apply(_.root.asRight)

  def down[A <: JAny, B <: JAny](lens: IdJLens[A, B]): IndexedState[A, B] = apply(lens(_).map(_.foci))
  def down[A <: JAny, B <: JAny](lens: CreatableJLens[A, B]): IndexedState[A, B] = down(lens.toIdJLens)

  def up[A <: JAny]: IndexedState[A, JAny] = apply(_.parent)

  def modify[A <: JAny, B <: JAny](fn: JFocus[A] => B): IndexedState[A, B] =
    IndexedStateT.apply[JResult, JFocus[A], JFocus[B], B] { fa =>
      val fb = fa.modify(fn)
      (fb -> fb.value).rightNec
    }

  def modifyF[A <: JAny, B <: JAny](fn: JFocus[A] => JResult[B]): IndexedState[A, B] =
    IndexedStateT.apply[JResult, JFocus[A], JFocus[B], B] { fa =>
      fa.modifyF(fn) map { fb =>
        fb -> fb.value
      }
    }

  def replace[A <: JAny, B <: JAny](value: B): IndexedState[A, B] =
    IndexedStateT.apply[JResult, JFocus[A], JFocus[B], B] { sin =>
      val sout = sin.replace(value)
      (sout -> sout.value).rightNec
    }

  def delete[A <: JAny]: IndexedState[A, JAny] = apply(_.delete())

  def encode[A](a: A)(implicit JAnyEncoder: JAnyEncoder[A]): IndexedState[Nothing, JAny] = replace(a.toJAny)

  def encodeTo[A <: JAny, B <: JAny, C](
      lens: CreatableJLens[A, B],
      value: C
  )(implicit encoder: Encoder[C, B]): State[A, B] =
    StateT.apply[JResult, JFocus[A], B] { fa =>
      fa.writeTo(lens, value.to[B]).map { fb =>
        // reroot this focus into the new document
        val fa2 = fa.replicate(fb.root.value).asInstanceOf[JFocus[A]]
        fa2 -> fb.value.asInstanceOf[B]
      }
    }

  def encodeTo[A <: JAny, B <: JAny, C](
      lens: CreatableJLens[A, B],
      valueOpt: Option[C]
  )(implicit encoder: Encoder[C, B]): State[A, Option[B]] =
    valueOpt match {
      case Some(value) => encodeTo(lens, value).map(Some(_))
      case None        => StateT.pure(None)
    }

  def decode[A](implicit Decoder: Decoder[JAny, A]): State[JAny, A] =
    StateT.inspectF[JResult, JFocus[JAny], A](_.decode[A])

  def decodeThrough[B] = new DecodeThroughState[B]

  class DecodeThroughState[B] {
    def apply[F[+_]: Traverse, A <: JAny](lens: JLens[F, JAny, A])(implicit decoder: Decoder[A, B]): State[JAny, F[B]] =
      StateT.inspectF[JResult, JFocus[JAny], F[B]] { in =>
        lens(in).flatMap(_.foci.traverse(_.decode[B]))
      }
  }

}
