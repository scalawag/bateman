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

/** Represents a JSON Pointer (https://tools.ietf.org/html/rfc6901). */

trait JPointer {

  /** Create a new JPointer corresponding to an index of this JSON array. */
  def /(index: Int): JPointer.Index = JPointer.Index(this, index)

  /** Create a new JPointer corresponding to a field of this JSON object. */
  def /(key: String): JPointer.Key = JPointer.Key(this, key)

  def navigate(from: JAny): DecodeResult[JAny]

  def navigateOption(from: JAny): DecodeResult[Option[JAny]] =
    navigate(from).fold(
      ee =>
        ee.iterator.toList match {
          case List(_: MissingValue) => None.validNec
          case _                     => ee.invalid
        },
      Some(_).validNec
    )
}

object JPointer {

  /** Represents a root JSON Pointer, normally corresponding to the single JSON object in a JSON text. */
  case object Root extends JPointer {
    override val toString: String = "/"
    override def navigate(from: JAny): DecodeResult[JAny] = from.validNec
  }

  sealed trait Child extends JPointer {
    val parent: JPointer
    val token: String

    override lazy val toString: String =
      this.parent match {
        case Root     => "/" + token
        case p: Child => p.toString + "/" + token
      }

  }

  /** Represents a non-root JSON Pointer, corresponding to a member within a JSON array. */
  final case class Index(parent: JPointer, index: Int) extends Child {
    override val token: String = index.toString
    override def navigate(from: JAny): DecodeResult[JAny] =
      parent.navigate(from).andThen(_.asArray.andThen(_.requiredIndex(index)))
  }

  /** Represents a non-root JSON Pointer, corresponding to a member within a JSON object. */
  final case class Key(parent: JPointer, key: String) extends Child {
    override val token: String = key // TODO: need escaping here

    override def navigate(from: JAny): DecodeResult[JAny] =
      parent.navigate(from).andThen(_.asObject.andThen(_(key)))

    def map(fn: String => String): Key = copy(key = fn(key))
  }
}
