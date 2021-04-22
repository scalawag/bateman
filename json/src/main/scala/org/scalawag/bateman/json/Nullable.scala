package org.scalawag.bateman.json

import cats.syntax.traverse._
import cats.syntax.functor._
import cats.{Applicative, Eval, Traverse}
import org.scalawag.bateman.json.decoding.JNull

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
  override def map[B](f: Nothing => B): Nullable[B] = this
}

object Null extends Null {
  def apply[A](src: JNull): Null = Sourced(src)
  final case class Sourced(src: JNull) extends Null
  override def toString: String = "Null"
}

final case class NotNull[+A](a: A) extends Nullable[A] {
  override val toOption: Option[A] = Some(a)
  override def map[B](f: A => B): Nullable[B] = NotNull(f(a))
}
