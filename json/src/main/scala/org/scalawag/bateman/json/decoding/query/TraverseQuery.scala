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

package org.scalawag.bateman.json.decoding.query

import cats.{Monad, Monoid, Traverse}
import cats.syntax.validated._
import cats.syntax.traverse._
import cats.syntax.functor._
import cats.syntax.flatMap._
import org.scalawag.bateman.json.decoding.{DecodeResult, ContextualDecoder, JAny, MissingValue}

// Trying to get Scala to follow all its possible avenues to get implicits for long query chains was a nightmare.
// This makes it so that each query knows how to traverse its output, giving us a way to build up a long chain one
// step at a time. This is something the compiler seems to be able to handle.

abstract class TraverseQuery[F[_], -From, To, -Context](implicit val traverse: Traverse[F])
    extends Function2[From, Context, DecodeResult[F[To]]]

object TraverseQuery {
  def apply[F[_]: Traverse, From, To, Context](
      fn: (From, Context) => DecodeResult[F[To]]
  ): TraverseQuery[F, From, To, Context] =
    new TraverseQuery[F, From, To, Context] {
      override def apply(from: From, context: Context): DecodeResult[F[To]] = fn(from, context)
    }

  implicit class RichTraverseQuery[F[_], -From, To, Context](q: TraverseQuery[F, From, To, Context]) {

    // TODO: Hack for 'as' - why can't this automatically convert to a query?
    def ~>[NewTo](as: AsTargetPlaceholder[NewTo])(implicit
        dec: ContextualDecoder[To, NewTo, Context]
    ): TraverseQuery[F, From, NewTo, Context] =
      new TraverseQuery[F, From, NewTo, Context]()(q.traverse) {
        override def apply(from: From, context: Context): DecodeResult[F[NewTo]] =
          q(from, context).andThen(_.traverse(dec.decode(_, context)))
      }

    /** The next query is non-traverse, so just use our traverse. */
    def ~>[NewTo](next: Query[To, NewTo, Context]): TraverseQuery[F, From, NewTo, Context] =
      new TraverseQuery[F, From, NewTo, Context]()(q.traverse) {
        override def apply(from: From, context: Context): DecodeResult[F[NewTo]] =
          q(from, context).andThen(_.traverse(next(_, context)))
      }

    /** The next query is a traverse query as well, so stack our traverses. */
    def ~>[G[_], NewTo](
        next: TraverseQuery[G, To, NewTo, Context]
    ): TraverseQuery[({ type H[X] = F[G[X]] })#H, From, NewTo, Context] = {
      implicit val fTraverse: Traverse[F] = q.traverse
      new TraverseQuery[({ type H[X] = F[G[X]] })#H, From, NewTo, Context]()(q.traverse compose next.traverse) {
        override def apply(from: From, context: Context): DecodeResult[F[G[NewTo]]] =
          q(from, context).andThen(_.traverse(next(_, context)))
      }
    }

    /** The next query is non-traverse and we want to treat it as optional, so produce an Option query. */
    def ~>?[NewTo](
        next: Query[To, NewTo, Context]
    ): TraverseQuery[({ type H[X] = F[Option[X]] })#H, From, NewTo, Context] = {
      implicit val fTraverse: Traverse[F] = q.traverse
      new TraverseQuery[({ type H[X] = F[Option[X]] })#H, From, NewTo, Context]()(fTraverse compose Traverse[Option]) {
        val oq: TraverseQuery[Option, To, NewTo, Context] = Query.optionize(next)
        override def apply(from: From, context: Context): DecodeResult[F[Option[NewTo]]] =
          q(from, context).andThen(_.traverse(oq(_, context)))
      }
    }
  }

  /** When this query has a FlatMap for its effect, we also unlock the ability to do star extensions (which
    * flatten the results). */

  implicit class RichFlatMapQuery[F[_], -From, To, Context](q: TraverseQuery[F, From, To, Context])(implicit
      fMonad: Monad[F]
  ) {

    /** Acts like a flatMap for Queries. Flattens the results into a single list. Requires that the input and output
      * are both wrapped in the same Traverse/FlatMap (usually List or Option).
      */
    def ~>*[NewTo](next: TraverseQuery[F, To, NewTo, Context]): TraverseQuery[F, From, NewTo, Context] =
      new TraverseQuery[F, From, NewTo, Context]()(q.traverse) {
        override def apply(from: From, context: Context): DecodeResult[F[NewTo]] = {
          q(from, context).andThen(fb => fb.traverse(next(_, context)).map(_.flatten))
        }
      }

//    def ~>*[C](next: TraverseQuery[Option, B, C]): TraverseQuery[F, A, C] = {
//      implicit val fTraverse: Traverse[F] = q.traverse
//
//      new TraverseQuery[F, A, C] {
//        override def apply(a: A): DecodeResult[F[C]] = {
//          q(a).andThen(fb => q.traverse.traverse(fb)(next).map(_.flatten))
//        }
//      }
//    }

    /** Extends with another query which we treat as optional and whose results are then flattened.
      * This basically means that anything that's missing just disappears without increasing the
      * traverse nesting.
      */
    def ~>?*[NewTo](
        next: Query[To, NewTo, Context]
    )(implicit fMonoid: Monoid[F[NewTo]]): TraverseQuery[F, From, NewTo, Context] = {
      new TraverseQuery[F, From, NewTo, Context]()(q.traverse) {
        val oq: TraverseQuery[Option, To, NewTo, Context] = Query.optionize(next)
        override def apply(from: From, context: Context): DecodeResult[F[NewTo]] =
          q(from, context).andThen(
            fMonad
              .map(_) { b =>
                oq(b, context) map {
                  case Some(c) => fMonad.pure(c)
                  case None    => fMonoid.empty
                }
              }
              .flatSequence[DecodeResult, NewTo]
          )
      }

//      (q ~> optionize(next))(_).map { foc =>
//        foc.flatMap {
//          case Some(c) => fApplicative.pure(c)
//          case none    => fMonoid.empty
//        }
//      }
//

      //      new TraverseQuery[({ type H[X] = F[G[X]] })#H, A, C] {
//        override def apply(a: A): DecodeResult[F[C]] = {
//          q(a).andThen(fb => q.traverse.traverse(fb)(next).map(_.flatten))
//        }
//      }
    }

  }
}
