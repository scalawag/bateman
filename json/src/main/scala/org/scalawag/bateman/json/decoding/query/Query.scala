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

import cats.Id
import cats.syntax.validated._
import cats.data.Validated.{Invalid, Valid}
import org.scalawag.bateman.json.decoding.{DecodeResult, ContextualDecoder, JAny, MissingValue}

// This exists because scala doesn't like Id[Id[_]], so we can't just arbitrarily nest Ids as Traverses.
// Having a specific type allows us to override the chaining behavior and hand-craft the resulting traverse so
// that we don't accidentally stumble upon that illegal nesting.
//
// It doesn't extend TraverseQuery because that makes the compiler complain about ambiguous overloads in some
// places.

trait Query[-From, To, -Context] extends Function2[From, Context, DecodeResult[To]] {
  // For cases when you really need a TraverseQuery...
  def toTraverseQuery: TraverseQuery[Id, From, To, Context] = TraverseQuery[Id, From, To, Context](this)
}

object Query {
  def apply[From, To, Context](fn: (From, Context) => DecodeResult[To]): Query[From, To, Context] = fn(_, _)

  implicit def stringToQuery[Context](name: String): Query[JAny, JAny, Context] = field(name)
  implicit def intToQuery[Context](n: Int): Query[JAny, JAny, Context] = index(n)

  def optionize[From, To, Context](q: Query[From, To, Context]): TraverseQuery[Option, From, To, Context] =
    new TraverseQuery[Option, From, To, Context] {
      override def apply(from: From, context: Context): DecodeResult[Option[To]] =
        q(from, context) match {
          case Valid(b) =>
            Some(b).validNec
          case Invalid(ee) if ee.forall(_.isInstanceOf[MissingValue]) =>
            None.validNec
          case Invalid(ee) =>
            ee.invalid
        }
    }

  /** Adds the arrow extension operators to a Query. */

  implicit class RichQuery[-From, To, Context](q: Query[From, To, Context]) {

    // TODO: Hack for 'as' - why can't this automatically convert to a query?
    def ~>[NewTo](as: AsTargetPlaceholder[NewTo])(implicit
        dec: ContextualDecoder[To, NewTo, Context]
    ): Query[From, NewTo, Context] = { (from, context) =>
      q(from, context).andThen(dec.decode(_, context))
    }

    /** The next query is non-traverse, so just produce another non-traverse query. */
    def ~>[NewTo](next: Query[To, NewTo, Context]): Query[From, NewTo, Context] = { (from, context) =>
      q(from, context).andThen(next(_, context))
    }

    /** The next query is non-traverse and we want to treat it as optional, so produce an Option query. */
    def ~>?[NewTo](next: Query[To, NewTo, Context]): TraverseQuery[Option, From, NewTo, Context] =
      new TraverseQuery[Option, From, NewTo, Context] {
        override def apply(from: From, context: Context): DecodeResult[Option[NewTo]] = {
          q(from, context).andThen(optionize(next)(_, context))
        }
      }

    /** The next query is a traverse, so use its traverse. */
    def ~>[G[_], NewTo](next: TraverseQuery[G, To, NewTo, Context]): TraverseQuery[G, From, NewTo, Context] =
      new TraverseQuery[G, From, NewTo, Context]()(next.traverse) {
        override def apply(from: From, context: Context): DecodeResult[G[NewTo]] =
          q(from, context).andThen(next(_, context))
      }
  }

}
