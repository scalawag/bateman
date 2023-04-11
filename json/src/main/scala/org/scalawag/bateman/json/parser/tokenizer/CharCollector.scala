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

package org.scalawag.bateman.json.parser.tokenizer

import cats.Eval
import cats.data.{EitherT, StateT}
import org.scalawag.bateman.json.parser.{SyntaxError, UnexpectedChars}

private[parser] trait CharCollector {
  protected type Result[A] = EitherT[Eval, SyntaxError, A]
  protected type State[A] = StateT[Result, CharStream, A]

  protected def syntaxError[A](expected: Predef.String): State[A] =
    StateT[Result, CharStream, A] { in =>
      EitherT.leftT(UnexpectedChars(in, expected))
    }

  protected val get: State[CharStream] = StateT.get[Result, CharStream]
  protected def pure[A](a: A): State[A] = StateT.pure[Result, CharStream, A](a)
  protected val peek: State[Option[Char]] = get.map(_.chars.headOption)
  protected val consume: State[Unit] = StateT.modify[Result, CharStream](_.drop(1))
}
