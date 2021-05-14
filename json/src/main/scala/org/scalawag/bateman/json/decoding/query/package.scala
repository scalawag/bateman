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

package org.scalawag.bateman.json.decoding

import cats.syntax.validated._

package object query {

  /** Starts off a query by anchoring the input type and context. */
  def root[FromTo, Context]: Query[FromTo, FromTo, Context] =
    Query { (from, _) =>
      from.validNec
    }

  def field[Context](name: String): Query[JAny, JAny, Context] =
    Query { (from, context) =>
      from.asObject.andThen(_.apply(name))
    }

  def index[Context](n: Int): Query[JAny, JAny, Context] =
    Query { (from, context) =>
      from.asArray.andThen(_.requiredIndex(n))
    }

  def all[Context]: TraverseQuery[List, JAny, JAny, Context] =
    TraverseQuery[List, JAny, JAny, Context] { (from, context) =>
      from.asArray.map(_.items.toList)
    }

  def *[Context]: TraverseQuery[List, JAny, JAny, Context] = all

  // Turn a Decoder into a Query
  class DecodeTargetPlaceholder[To] {
    def apply[From, Context](d: ContextualDecoder[From, To, Context]): Query[From, To, Context] = Query(d.decode)
  }

  def decode[To] = new DecodeTargetPlaceholder[To]

  /** Attempts to convert the input type to the output type using an appropriate Decoder. */
  def as[To]: AsTargetPlaceholder[To] = new AsTargetPlaceholder[To]

  class AsTargetPlaceholder[To]

  /** Gives you a handy query method for any class. */

  implicit class Queryable[From](from: From) {
    // Since all these are functions and erase to the same time, they have to be named differently. If I try to
    // convert them to a magnet, there's not enough context for scala to guess at the function type and make the
    // leading "_ ~> " useful. It seemed like the best alternative was to just have different names for the different
    // variations. Ideally, these would all be named "query."

    def query[To](fn: Query[From, From, Any] => Query[From, To, Any]): DecodeResult[To] =
      fn(root[From, Any])(from, ())

    def tquery[F[_], To](fn: Query[From, From, Any] => TraverseQuery[F, From, To, Any]): DecodeResult[F[To]] =
      fn(root[From, Any])(from, ())

    def cquery[To, Context](
        context: Context
    )(fn: Query[From, From, Context] => Query[From, To, Context]): DecodeResult[To] =
      fn(root[From, Context])(from, context)

    def ctquery[F[_], To, Context](
        context: Context
    )(fn: Query[From, From, Context] => TraverseQuery[F, From, To, Context]): DecodeResult[F[To]] =
      fn(root[From, Context])(from, context)

    def extract[To, Context](q: Query[From, To, Context], context: Context): DecodeResult[To] =
      q(from, context)

    def extract[F[_], To, Context](q: TraverseQuery[F, From, To, Context], context: Context): DecodeResult[F[To]] =
      q(from, context)
  }
}
