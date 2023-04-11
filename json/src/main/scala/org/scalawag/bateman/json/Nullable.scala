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

import cats.syntax.traverse._
import cats.syntax.functor._
import cats.syntax.parallel._
import cats.{Applicative, Eval, Traverse}

sealed trait Nullable[+A] {
  def toOption: Option[A]
  def map[B](f: A => B): Nullable[B]
}

object Nullable {
  def apply[A](a: Option[A]): Nullable[A] = a.map(NotNull(_)).getOrElse(Null)

  implicit val traverse: Traverse[Nullable] = new Traverse[Nullable] {
    override def traverse[G[_], A, B](fa: Nullable[A])(f: A => G[B])(implicit G: Applicative[G]): G[Nullable[B]] =
      fa.toOption.traverse(f).map(Nullable(_))
    override def foldLeft[A, B](fa: Nullable[A], b: B)(f: (B, A) => B): B =
      fa.toOption.foldLeft(b)(f)
    override def foldRight[A, B](fa: Nullable[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
      fa.toOption.foldRight(lb)(f)
  }
}

sealed trait Null extends Nullable[Nothing] {
  override def toOption: Option[Nothing] = None
  override def map[B](f: Nothing => B): Nullable[B] = this // TODO?
}

case object Null extends Null

final case class NotNull[+A](value: A) extends Nullable[A] {
  override val toOption: Option[A] = Some(value)
  override def map[B](f: A => B): Nullable[B] = NotNull(f(value))
}
