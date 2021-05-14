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

import shapeless.{::, HList, HNil, Witness}
import shapeless.labelled.FieldType
import scala.reflect.runtime.universe.WeakTypeTag

trait Dumper[R] {
  def dump(r: R): List[Dumper.Item]
}

object Dumper {
  def dump[A](a: A)(implicit dumper: Dumper[A]): List[Item] = dumper.dump(a)

  implicit val hnilDumper: Dumper[HNil] = { _ => Nil }

  case class Item(key: Option[Symbol], typeTag: WeakTypeTag[_], value: Any) {
    override def toString: String = s"${key.map(_.name).getOrElse("???")}: ${typeTag.tpe} -> $value"
  }

  implicit def hconsLabelledDumper[K <: Symbol, H, T <: HList](implicit
      k: Witness.Aux[K],
      h: WeakTypeTag[H],
      tailDumper: Dumper[T]
  ): Dumper[FieldType[K, H] :: T] = { in =>
    Item(Some(k.value), h, in.head) :: tailDumper.dump(in.tail)
  }

  implicit def hconsDumper[H, T <: HList](implicit
      h: WeakTypeTag[H],
      tailDumper: Dumper[T]
  ): Dumper[H :: T] = { in =>
    Item(None, h, in.head) :: tailDumper.dump(in.tail)
  }
}
