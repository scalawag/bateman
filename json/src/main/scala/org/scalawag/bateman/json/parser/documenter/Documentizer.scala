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

package org.scalawag.bateman.json.parser.documenter

import cats.Eval
import cats.data.{EitherT, StateT}
import org.scalawag.bateman.json.JNull.JNullImpl
import org.scalawag.bateman.json.parser.SyntaxError
import org.scalawag.bateman.json.parser.tokenizer.{False, Null, NumberToken, PrimitiveToken, StringToken, True}
import org.scalawag.bateman.json.parser.eventizer.{
  ArrayEnd,
  ArrayStart,
  Event,
  Eventizer,
  FieldStart,
  ObjectEnd,
  ObjectStart,
  Value
}
import org.scalawag.bateman.json.{JAny, JArray, JBoolean, JField, JLocation, JNull, JNumber, JObject, JPointer, JString}

import scala.collection.compat.immutable.LazyList

/** By the time we get here, any errors should have already been detected. Since the event stream is another
  * possible integration point for consumers, the contents have to be specific. So, the only error detected
  * here needs to be a SyntaxError in the input stream (and that can simply be "rethrown" -- emitted as-is).
  */

object Documentizer {
  private case class EventStream(events: LazyList[Either[SyntaxError, Event]])
  type OutStream = LazyList[Either[SyntaxError, JAny]]

  type MaybeError[A] = EitherT[Eval, SyntaxError, A]
  private type State[A] = StateT[MaybeError, EventStream, A]

  private val get: State[EventStream] = StateT.get[MaybeError, EventStream]
  private def pure[A](a: A): State[A] = StateT.pure[MaybeError, EventStream, A](a)
  private val peek: State[Event] = StateT[MaybeError, EventStream, Event] { es =>
    es.events.head match {
      case Left(err)  => EitherT.leftT(err)
      case Right(evt) => EitherT.rightT(es -> evt)
    }
  }

  private val consume: State[Unit] = StateT.modify[MaybeError, EventStream] { in =>
    in.copy(events = in.events.drop(1))
  }

  private def make[A](t: PrimitiveToken)(ctor: Some[JLocation] => A) =
    for {
      _ <- get
    } yield ctor(Some(t.location))

  private def items(index: Int): State[List[JAny]] =
    peek flatMap {
      case ArrayEnd(_) =>
        for {
          _ <- consume
        } yield Nil

      case _ =>
        for {
          h <- any
          t <- items(index + 1)
        } yield h :: t
    }

  private def arr(start: ArrayStart): State[JAny] =
    for {
      ff <- items(0)
    } yield JArray(ff, Some(start.token.location))

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
          k <- make(name)(JString(name.value, _))
          h <- any
          _ <- consume // TODO: validate FieldEnd
          t <- fields
        } yield JField(k, h) :: t

      case _ => ??? // avoid warning
    }

  private def obj(start: ObjectStart): State[JAny] =
    for {
      ff <- fields
    } yield JObject(ff, Some(start.token.location))

  private val any: State[JAny] = {
    peek flatMap {
      case Value(e: Null)        => consume.flatMap(_ => make(e)(JNullImpl(_)))
      case Value(e: True)        => consume.flatMap(_ => make(e)(JBoolean(true, _)))
      case Value(e: False)       => consume.flatMap(_ => make(e)(JBoolean(false, _)))
      case Value(e: StringToken) => consume.flatMap(_ => make(e)(JString(e.value, _)))
      case Value(e: NumberToken) => consume.flatMap(_ => make(e)(new JNumber(e.value, _)))

      case e: ArrayStart  => consume.flatMap(_ => arr(e))
      case e: ObjectStart => consume.flatMap(_ => obj(e))
      case _              => ??? // avoid warning
    }
  }

  private def anys(in: EventStream): LazyList[MaybeError[JAny]] =
    if (in.events.isEmpty)
      LazyList.empty
    else
      any.run(in).value.value match {
        case Right((next, v)) => EitherT[Eval, SyntaxError, JAny](Eval.always(Right(v))) #:: anys(next)
        case Left(e)          => LazyList(EitherT.leftT(e))
      }

  def documentize(in: Eventizer.EventStream): OutStream = anys(EventStream(in)).map(_.value.value)
}
