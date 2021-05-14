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

package org.scalawag.bateman.json.generic

import cats.syntax.validated._
import org.scalawag.bateman.json.decoding.ContextualDecoder
import org.scalawag.bateman.json.encoding.Encoder
import org.scalawag.bateman.json.validating.Validator
import shapeless.{Lazy, tag}
import shapeless.tag.@@

/**
  * This is the base trait for tags used to mark case class fields for special handling. It's convenient, though
  * not required, that each tag you define have a companion object deriving from [[TagCompanion]]. If you don't
  * do this, you'll probably still want to provide the same implicit definitions somehow.
  *
  * The tags you define should extends this trait and not add any other constraints (abstract members).
  */

trait Tag

/**
  * Each [[Tag]] is expected to have certain type class instances available for it. This base class can be extended
  * by its companion object to generate those instances.
  */

class TagCompanion[T] {

  /**
    * Since the tags have no abstract members, they can be freely applied to any value. This implicit automatically
    * wraps values with the tag. This makes it possible for developers to assign values to the case class fields
    * without having to explicitly wrap them.
    */

  implicit def autoTag[A](a: A): A @@ T = tag[T](a)

  /**
    * Generates a tagged [[ContextualDecoder]] based on the corresponding untagged [[ContextualDecoder]]. This means that no special
    * decoder is required for types tagged with this [[Tag]]. It simply runs the underlying [[ContextualDecoder]] and then
    * wraps the output with the [[Tag]].
    */

  def decoder[A, B, Context](implicit
      dec: Lazy[ContextualDecoder[A, B, Context]]
  ): ContextualDecoder[A, B @@ T, Context] =
    dec.value.decode(_, _).map(autoTag)

  /**
    * Generates a tagged [[Encoder]] based on the corresponding untagged [[Encoder]]. This means that no special
    * encoder is required for types tagged with this [[Tag]]. Tags have no bearing on encoding, so this just defers
    * to the underlying [[ContextualDecoder]].
    */

  def encoder[A, B](implicit enc: Lazy[Encoder[A, B]]): Encoder[A @@ T, B] =
    enc.value.encode(_)

  /**
    * In case it's more convenient to validate _every_ field decooded for a case class than to keep track of which
    * ones support validation, this provides a validator that always passes and simply tags the input.
    */

  implicit def validator[A]: Validator[A, A @@ T] = autoTag(_).validNec
}
