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

import org.scalatest.funspec.AnyFunSpec
import shapeless.{LabelledGeneric, Poly1}
import shapeless.labelled.{FieldType, field}
import shapeless.ops.hlist.{Mapper, ToTraversable}
import shapeless.ops.record.Keys
import shapeless.tag.@@

class SimpleTest extends AnyFunSpec {
  it("should") {
    import shapeless._

    trait Attribute
    implicit def addAttribute[A](a: A): A @@ Attribute = tag[Attribute](a)
    trait Relationship
    implicit def addRelationship[A](a: A): A @@ Relationship = tag[Relationship](a)

    object optionize extends Poly1 {
      implicit def genericCase[K <: Symbol, H]: Case.Aux[FieldType[K, H], FieldType[K, Option[H]]] =
        at(a => field[K](Option(a)))
    }

    // TODO: make a small test that just tries to run optionize against an HList and see what comes out.
    case class A(a: Int @@ Attribute, b: Int @@ Relationship, c: Option[Int] @@ Relationship)
    val a = A(8, 17, None)

    def go[A, B <: HList, C <: HList](
        a: A
    )(implicit gen: LabelledGeneric.Aux[A, B], m: Mapper.Aux[optionize.type, B, C]): C = {
      m(gen.to(a))
    }

    def go2[C <: HList, D <: HList](
        c: C
    )(implicit keys: Keys.Aux[C, D], trav: ToTraversable.Aux[D, List, Symbol]) = {
      keys().toList
    }

    val c = go(a)
    println(s"C: $c")
    val k = go2(c)
    println(s"K: $k")

    trait TC[A] {
      def apply(a: A): String
    }

    object TC {
      def go[A](a: A)(implicit tc: TC[A]): String = tc(a)
    }
    implicit def hnil: TC[HNil] = { _ => "HNil" }
    implicit def attr[K <: Symbol, V, C <: HList, T <: HList](implicit
        tc: TC[T]
    ): TC[FieldType[K, Option[V @@ Attribute]] :: T] = { in =>
      in.head.toString + " -> " + tc(in.tail)
    }
    implicit def rela[K <: Symbol, V, C <: HList, T <: HList](implicit
        tc: TC[T]
    ): TC[FieldType[K, Option[V @@ Relationship]] :: T] = { in =>
      in.head.toString.reverse + " -> " + tc(in.tail)
    }

    val e = TC.go(c)
    println(e)
  }
}
