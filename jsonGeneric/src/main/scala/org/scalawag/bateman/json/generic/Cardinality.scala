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

import cats.{Traverse, catsInstancesForId}
import org.scalawag.bateman.json.Nullable
import org.scalawag.bateman.json.focus.Single

trait Cardinality[F[_]] {
  implicit val traverse: Traverse[F]
}

object Cardinality {
  implicit val forId: Cardinality[Single] = new Cardinality[Single] {
    override val traverse: Traverse[Single] = catsInstancesForId
  }
  implicit val forNullable: Cardinality[Nullable] = new Cardinality[Nullable] {
    override val traverse: Traverse[Nullable] = Nullable.traverse
  }
  implicit val forList: Cardinality[List] = new Cardinality[List] {
    override val traverse: Traverse[List] = cats.instances.list.catsStdInstancesForList
  }

  implicit def traverseForCardinality[F[_]](implicit card: Cardinality[F]): Traverse[F] = card.traverse
}
