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

import scala.compiletime.*
import scala.deriving.Mirror
import scala.reflect.ClassTag

object TraitUtils:
  /** Recursively flatten a tuple of subtypes, expanding any element that is itself a sealed trait
    * into its (transitive) leaf subtypes. For each leaf, summons an instance of the requested type
    * class `F[t]` along with a `ClassTag[t]`.
    *
    * The result is deduped by runtime class, so a concrete leaf reachable via multiple sealed-trait
    * branches (diamond inheritance) appears only once.
    *
    * Trait encoder/decoder factories use this to build a flat-leaf dispatch table even when the
    * trait being derived has intermediate sealed-trait subtypes — callers may then provide
    * leaf-only `CustomDiscriminator` mappings, while layered mappings (`forType[Intermediate]`)
    * still work via `isAssignableFrom` matching during the per-leaf discriminator query.
    *
    * Type class instances and `ClassTag`s are summoned together in a single recursive pass to keep
    * the generated tree shallow enough for the JS compiler's default stack (which is smaller than
    * on JVM).
    */
  inline def summonConcretes[T <: Tuple, F[_]]: List[(F[Any], ClassTag[?])] = {
    inline erasedValue[T] match
      case _: EmptyTuple => Nil
      case _: (t *: ts) =>
        val tail = summonConcretes[ts, F]
        summonFrom {
          case innerM: Mirror.SumOf[`t`] =>
            summonConcretes[innerM.MirroredElemTypes, F] ::: tail
          case _ =>
            (summonInline[F[t]].asInstanceOf[F[Any]], summonInline[ClassTag[t]]) :: tail
        }
  }.distinctBy(_._2.runtimeClass)
