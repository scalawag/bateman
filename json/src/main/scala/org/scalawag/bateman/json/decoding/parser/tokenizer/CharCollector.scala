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

package org.scalawag.bateman.json.decoding.parser.tokenizer

import cats.data.StateT
import cats.syntax.either._
import org.scalawag.bateman.json.decoding.parser.SyntaxError

private[parser] trait CharCollector {
  protected type Result[A] = Either[SyntaxError, A]
  protected type State[A] = StateT[Result, CharStream, A]

  protected def syntaxError[A](reason: Predef.String): State[A] =
    StateT[Result, CharStream, A] { in =>
      SyntaxError(in, reason).asLeft
    }

  protected val get: StateT[Result, CharStream, CharStream] = StateT.get[Result, CharStream]
  protected def pure[A](a: A): StateT[Result, CharStream, A] = StateT.pure[Result, CharStream, A](a)
  protected val peek: State[Option[Char]] = get.map(_.chars.headOption)
  protected val consume: StateT[Result, CharStream, Unit] = StateT.modify[Result, CharStream](_.drop(1))
}
