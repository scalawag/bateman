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

package org.scalawag.bateman.json.decoding.parser.documenter

import cats.Eval
import cats.data.{EitherT, StateT}
import org.scalawag.bateman.json.decoding.parser.SyntaxError
import org.scalawag.bateman.json.decoding.parser.tokenizer.{False, Null, NumberToken, PrimitiveToken, StringToken, True}
import org.scalawag.bateman.json.decoding.parser.eventizer.{
  ArrayEnd,
  ArrayStart,
  Event,
  Eventizer,
  FieldStart,
  ObjectEnd,
  ObjectStart,
  Value
}
import org.scalawag.bateman.json.decoding.parser.tokenizer.NumberToken
import org.scalawag.bateman.json.decoding.{
  JAny,
  JArray,
  JBoolean,
  JField,
  JLocation,
  JNull,
  JNumber,
  JObject,
  JPointer,
  JString,
  parser
}

/** By the time we get here, any errors should have already been detected. Since the event stream is another
  * possible integration point for consumers, the contents have to be specific. So, the only error detected
  * here needs to be a SyntaxError in the input stream (and that can simply be "rethrown" -- emitted as-is).
  */

object Documentizer {
  private case class EventStream(events: Stream[Either[SyntaxError, Event]], pointer: JPointer)
  type OutStream = Stream[Either[SyntaxError, JAny]]

  type MaybeError[A] = EitherT[Eval, SyntaxError, A]
  private type State[A] = StateT[MaybeError, EventStream, A]

  private val get: State[EventStream] = StateT.get[MaybeError, EventStream]
  private def pure[A](a: A): State[A] = StateT.pure[MaybeError, EventStream, A](a)
  private val peek: State[Event] = StateT[MaybeError, EventStream, Event] {
    case EventStream(Left(err) #:: _, _)       => EitherT.leftT(err)
    case in @ EventStream(Right(evt) #:: _, _) => EitherT.rightT(in -> evt)
  }

  private val consume: State[Unit] = StateT.modify[MaybeError, EventStream] { in =>
    in.copy(events = in.events.drop(1))
  }

  private def descend(key: String) =
    StateT.modify[MaybeError, EventStream] { in =>
      in.copy(pointer = in.pointer / key)
    }

  private def descend(index: Int) =
    StateT.modify[MaybeError, EventStream] { in =>
      in.copy(pointer = in.pointer / index)
    }

  private val ascend =
    StateT.modify[MaybeError, EventStream] { in =>
      in.pointer match {
        case c: JPointer.Child => in.copy(pointer = c.parent)
        case _ =>
          throw new IllegalArgumentException(
            "unbalanced events detected on input stream (ObjectEnd without ObjectStart)"
          )
      }
    }

  private def make[A](t: PrimitiveToken)(ctor: (JLocation, JPointer) => A) = {
    for {
      in <- get
    } yield ctor(t.position, in.pointer)
  }

  private def items(index: Int): State[List[JAny]] =
    peek flatMap {
      case ArrayEnd(_) =>
        for {
          _ <- consume
        } yield Nil

      case _ =>
        for {
          _ <- descend(index)
          h <- any
          _ <- ascend
          t <- items(index + 1)
        } yield h :: t
    }

  private def arr(start: ArrayStart): State[JAny] =
    for {
      ff <- items(0)
      p <- get.map(_.pointer)
    } yield JArray(ff, start.token.position, p)

  private val fields: State[List[JField]] =
    peek flatMap {
      case ObjectEnd(_) =>
        for {
          _ <- consume
          t <- pure(Nil)
        } yield t

      case FieldStart(name) =>
        for {
          _ <- consume
          _ <- descend(name.value)
          k <- make(name)(JString(name.value, _, _))
          h <- any
          _ <- consume // TODO: validate FieldEnd
          _ <- ascend
          t <- fields
        } yield JField(k, h) :: t
    }

  private def obj(start: ObjectStart): State[JAny] =
    for {
      ff <- fields
      p <- get.map(_.pointer)
    } yield JObject(ff, start.token.position, p)

  private val any: State[JAny] = {
    peek flatMap {
      case Value(e: Null)        => consume.flatMap(_ => make(e)(JNull.apply))
      case Value(e: True)        => consume.flatMap(_ => make(e)(JBoolean(true, _, _)))
      case Value(e: False)       => consume.flatMap(_ => make(e)(JBoolean(false, _, _)))
      case Value(e: StringToken) => consume.flatMap(_ => make(e)(JString(e.value, _, _)))
      case Value(e: NumberToken) => consume.flatMap(_ => make(e)(JNumber(e.value, _, _)))

      case e: ArrayStart  => consume.flatMap(_ => arr(e))
      case e: ObjectStart => consume.flatMap(_ => obj(e))
    }
  }

  private def anys(in: EventStream): Stream[MaybeError[JAny]] =
    if (in.events.isEmpty)
      Stream.Empty
    else
      any.run(in).value.value match {
        case Right((next, v)) => EitherT[Eval, SyntaxError, JAny](Eval.always(Right(v))) #:: anys(next)
        case Left(e)          => Stream(EitherT.leftT(e))
      }

  def documentize(in: Eventizer.EventStream): OutStream = anys(EventStream(in, JPointer.Root)).map(_.value.value)
}
