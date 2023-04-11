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

import cats.Order

import scala.annotation.tailrec

/** Represents a JSON Pointer (https://tools.ietf.org/html/rfc6901). */

case class JPointer(tokens: List[JPointer.Token]) {

  /** Create a new JPointer corresponding to an index of this JSON array. */
  def item(index: Int): JPointer = JPointer(this.tokens :+ JPointer.Index(index))

  /** Create a new JPointer corresponding to a field of this JSON object. */
  def field(key: String): JPointer = JPointer(this.tokens :+ JPointer.Key(key))

  def parent: JPointer = JPointer(this.tokens.init)
  override lazy val toString: String = tokens.map(_.token).mkString("/", "/", "")
}

object JPointer {
  val Root: JPointer = JPointer(Nil)

  object Leaf {
    def unapply(pointer: JPointer): Option[Token] = pointer.tokens.lastOption
  }

  sealed trait Token {
    val token: String
  }

  /** Represents a non-root JSON Pointer, corresponding to a member within a JSON array. */
  final case class Index(index: Int) extends Token {
    override val token: String = index.toString
  }

  /** Represents a non-root JSON Pointer, corresponding to a member within a JSON object. */
  final case class Key(key: String) extends Token {
    private val tilde = Iterable('~', '0')
    private val slash = Iterable('~', '1')
    override val token: String = key.iterator.flatMap {
      case '~' => tilde
      case '/' => slash
      case c   => Iterable(c)
    }.mkString
  }

  implicit val ordering: Ordering[JPointer] = new Ordering[JPointer] {
    override def compare(l: JPointer, r: JPointer): Int = {
      @tailrec
      def go(lt: List[Token], rt: List[Token]): Int =
        (lt, rt) match {
          case (Index(ln) :: lt, Index(rn) :: rt) =>
            val d = ln compare rn
            if (d != 0) d else go(lt, rt)
          case (Key(ln) :: lt, Key(rn) :: rt) =>
            val d = ln compare rn
            if (d != 0) d else go(lt, rt)
          case (Index(_) :: _, _) => -1
          case (_, Index(_) :: _) => 1
          case (Nil, Nil)         => 0
          case (Nil, _)           => -1
          case (_, Nil)           => 1
        }

      go(l.tokens, r.tokens)
    }
  }
  implicit val order: Order[JPointer] = Order.fromOrdering
}
