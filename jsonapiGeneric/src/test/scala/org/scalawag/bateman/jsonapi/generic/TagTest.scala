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

package org.scalawag.bateman.jsonapi.generic

import cats.Id
import shapeless.tag
import shapeless.tag.@@

object TagTest {
  trait A
  trait B

  def main(args: Array[String]): Unit = {
    import shapeless._
    import shapeless.tag._

    sealed trait Tag

    trait TypeClass[A, B] {
      def apply(a: A): B
    }

    object TypeClass {
      implicit def autoTag[A](a: A): A @@ Tag = tag[Tag](a)

      implicit val intTypeClass: TypeClass[Int, String] = _.toString

      implicit def taggedTypeClass[A, B](implicit tc: Lazy[TypeClass[A, B]]): TypeClass[A, B @@ Tag] = { a =>
        tag[Tag](tc.value(a))
      }
    }

//    implicit def recurse[A, B](implicit tc: Lazy[TypeClass[A, B]]): TypeClass[A, B] = tc.value

//    implicit def taggedIntTypeClass = taggedTypeClass[Int]

    println(implicitly[TypeClass[Int, String @@ Tag]].apply(8))
  }
}
