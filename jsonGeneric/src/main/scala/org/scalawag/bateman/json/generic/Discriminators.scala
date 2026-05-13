// bateman -- Copyright 2021-2026 -- Justin Patterson
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

import org.scalawag.bateman.json.JErrorFormatters._
import org.scalawag.bateman.json.{JAny, JAnyEncoder, ProgrammerError}
import org.scalawag.bateman.json.syntax._
import scala.reflect.{ClassTag, classTag}

object Discriminators {

  /** Contains the results of running a given type though the discriminator.
    *
    * @param value the discriminator value to use for the specified type
    * @param explicit the ''optional'' abstract instance to use (instead of the concrete instance) -- this is required
    *                 for use cases with layered discriminators
    * @tparam F the type class (encoder/decoder/codec) being targeted
    * @tparam A the type for which to find the mapping
    */
  case class DiscriminatorMapping[F[_], A: ClassTag](value: JAny, explicit: Option[F[A]]) {
    val classTag: ClassTag[A] = implicitly
  }

  /** Determines the appropriate mapping (discriminator value + instance) to be used for a specified data type.
    *
    * @tparam F the type class (encoder/decoder/codec) being targeted
    */
  trait Discriminator[F[_]] {
    // Whether multiple types returning the same discriminator value is an error.
    def duplicateValuesForbidden: Boolean
    def apply[A: ClassTag](implicit config: Config, default: F[A]): DiscriminatorMapping[F, A]
  }

  /** The default discriminator strategy. Derives the discriminator value automatically from the concrete type's
    * class name by taking the last word (after any `$` or `.` separators) and running it through
    * [[Config.classNameMapping]]. For example, `com.example.MyEvent$Completed` becomes `"Completed"` (or
    * `"completed"` with snake_case mapping).
    *
    * This discriminator forbids duplicate values — if two concrete types produce the same discriminator value
    * (e.g., identically-named classes in different packages), derivation fails at construction time.
    *
    * Never returns an explicit type class instance, so the implicitly-resolved encoder/decoder for each
    * concrete type is always used.
    */
  class SimpleClassNameDiscriminator[F[_]] extends Discriminator[F] {
    override val duplicateValuesForbidden: Boolean = true
    override def apply[A: ClassTag](implicit config: Config, default: F[A]): DiscriminatorMapping[F, A] = {
      val lastClassNameWord = classTag[A].runtimeClass.getName.split("\\W").last
      val value = config.classNameMapping(lastClassNameWord).toJAny
      DiscriminatorMapping(value, None)
    }
  }

  object SimpleClassNameDiscriminator {
    def apply[F[_]]: SimpleClassNameDiscriminator[F] = new SimpleClassNameDiscriminator[F]
  }

  /** A discriminator strategy that uses caller-provided mappings to determine discriminator values for each
    * concrete type. This is needed when:
    *   - The automatic class name mapping is insufficient (e.g., you want custom discriminator values)
    *   - You have a multi-level sealed trait hierarchy and need layered discriminators with explicit
    *     encoders/decoders that route through intermediate trait encoders
    *
    * Each mapping is a [[DiscriminatorMapper]] created via [[forType]], which associates a concrete type
    * with a discriminator value and optionally captures an explicit type class instance. When an explicit
    * instance is provided (e.g., a trait encoder for an intermediate sealed trait), it is used instead of
    * the default implicit instance, enabling nested discrimination.
    *
    * By default, duplicate discriminator values are forbidden. Use the two-argument constructor to allow
    * them (e.g., when multiple concrete types intentionally share a discriminator because they are
    * distinguished by a second discriminator at a deeper level).
    */
  case class CustomDiscriminator[F[_], A: ClassTag](duplicateValuesForbidden: Boolean)(
      mappers: DiscriminatorMapper[F, _ <: A]*
  ) extends Discriminator[F] {
    override def apply[B: ClassTag](implicit config: Config, default: F[B]): DiscriminatorMapping[F, B] = {
      // Scala 2.12 requires at least some of this nonsense.
      mappers.toList.map(_.apply(classTag[B]).toList).flatten[DiscriminatorMapping[F, _ <: A]] match {
        case List(v) => v.asInstanceOf[DiscriminatorMapping[F, B]]
        case Nil     => throw MissingDiscriminatorMapping[B]()
        case vv      => throw MultipleDiscriminatorMappings[B](vv.map(_.value))
      }
    }
  }

  object CustomDiscriminator {
    def apply[F[_], A: ClassTag](mappers: DiscriminatorMapper[F, _ <: A]*): CustomDiscriminator[F, A] =
      CustomDiscriminator(duplicateValuesForbidden = true)(mappers: _*)
  }

  /** Associates a concrete type `A` with a discriminator value and captures the type class instance `F[A]`.
    * Used within [[CustomDiscriminator]] mappings. When the discriminator is asked about a type `B`, this
    * mapper responds if `A` is assignable from `B` (i.e., `B` is `A` or a subtype of `A`). This subtype
    * matching is what enables layered discriminators: a mapper for an intermediate sealed trait will match
    * all of that trait's concrete subtypes, routing them through the trait's own encoder/decoder.
    */
  case class DiscriminatorMapper[F[_], A: ClassTag](value: JAny)(implicit F: F[A]) {
    def apply[B: ClassTag]: Option[DiscriminatorMapping[F, A]] = {
      val a: ClassTag[A] = classTag[A]
      val b: ClassTag[B] = classTag[B]
      if (a.runtimeClass.isAssignableFrom(b.runtimeClass))
        Some(DiscriminatorMapping(value, Some(implicitly[F[A]])))
      else
        None
    }
  }

  /** Entry point for building [[DiscriminatorMapper]] instances in a [[CustomDiscriminator]].
    *
    * Usage: `forType[MyConcreteType]("my_discriminator_value")` or, for layered discriminators,
    * `forType[MyIntermediateTrait]("my_discriminator_value")` where an implicit encoder/decoder
    * for the intermediate trait is in scope.
    */
  def forType[A] = new ForTypeBuilder[A]

  class ForTypeBuilder[A] {
    def apply[F[_], B: JAnyEncoder](value: B)(implicit A: ClassTag[A], F: F[A]): DiscriminatorMapper[F, A] =
      DiscriminatorMapper[F, A](value.toJAny)
  }
}

case class MissingDiscriminatorMapping[A: ClassTag]()
    extends ProgrammerError(
      s"missing discriminator mapping for ${classTag[A]}"
    ) {
  val forType: ClassTag[A] = classTag[A]
}

case class MultipleDiscriminatorMappings[A: ClassTag](values: List[JAny])
    extends ProgrammerError(
      s"Multiple discriminator mappings for type ${classTag[A]}: ${values.map(_.render).mkString(", ")}"
    ) {
  val forType: ClassTag[A] = classTag[A]
}

case class DiscriminatorCollision(discriminators: Map[JAny, List[ClassTag[_]]])
    extends ProgrammerError({
      maybeBullets(
        discriminators.map {
          case (value, types) =>
            s"types ${formatAndList(types.map(_.toString).iterator)} have the same discriminator value: '${value.render}'"
        }
      )
    })

object DiscriminatorCollision {
  import Discriminators.DiscriminatorMapping

  /** Throws [[DiscriminatorCollision]] when two or more mappings share a discriminator value
    * unless every mapping sharing that value has the *same* explicit type class instance.
    *
    * The exception is how layered discriminators work: a single `forType[IntermediateTrait](...)`
    * mapper matches every concrete leaf under the intermediate via `isAssignableFrom`, and each
    * match returns a [[DiscriminatorMapping]] wrapping the same explicit encoder/decoder. Those
    * mappings are a single logical mapping, not a collision.
    */
  def detect[F[_]](mappings: List[DiscriminatorMapping[F, _]]): Unit = {
    // Project to a stable tuple form before chaining; Scala 2.12's existential type inference is
    // shaky on `List[DiscriminatorMapping[F, _]]` once it hits groupBy/filter/map.
    val projected: List[(JAny, ClassTag[_], Option[Any])] =
      mappings.map(m => (m.value, m.classTag, m.explicit))
    val conflicts = projected
      .groupBy(_._1)
      .filter {
        case (_, group) =>
          group.size > 1 && {
            val explicits = group.map(_._3)
            // Dedupe by reference identity, not equals: two distinct explicit instances that
            // happen to be structurally equal (e.g., the F[_] type overrides equals) should still
            // be treated as a collision.
            explicits.exists(_.isEmpty) ||
            explicits.flatten.map(System.identityHashCode).distinct.size > 1
          }
      }
      .map { case (value, group) => value -> group.map(_._2) }
    if (conflicts.nonEmpty)
      throw DiscriminatorCollision(conflicts)
  }
}
