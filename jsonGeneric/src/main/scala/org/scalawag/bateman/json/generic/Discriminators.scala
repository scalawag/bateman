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

package org.scalawag.bateman.json.generic

import scala.language.higherKinds
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
  case class DiscriminatorMapping[F[_], A](value: JAny, explicit: Option[F[A]])

  /** Determines the appropriate mapping (discriminator value + instance) to be used for a specified data type.
    *
    * @tparam F the type class (encoder/decoder/codec) being targeted
    */
  trait Discriminator[F[_]] {
    // Whether multiple types returning the same discriminator value is an error.
    def duplicateValuesForbidden: Boolean
    def apply[A: ClassTag](implicit config: Config, default: F[A]): DiscriminatorMapping[F, A]
  }

  /** A discriminator implementation that just uses the last word of the class name of the concrete type and puts
    * it into the location specified by the lens. It never returns an explicit type class instance because it has
    * no means of acquiring one.
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
  // This function is executed purely for the side effect of throwing a ProgrammerError, if appropriate.
  def detect(discriminators: Map[JAny, List[ClassTag[_]]]): Unit = {
    val dups = discriminators.filter(_._2.size > 1)
    if (dups.nonEmpty)
      throw DiscriminatorCollision(dups)
    else
      ()
  }
}
