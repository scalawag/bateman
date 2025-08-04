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

trait MemberLabels[T]:
  def apply(): List[String]

object MemberLabels:
  inline def summonLabels[T](using m: Mirror.Of[T]): MemberLabels[T] =
    val labels = constValueTuple[m.MirroredElemLabels].toArray.map(_.toString).toList
    class MemberLabelsImpl(labels: List[String]) extends MemberLabels[T]:
      def apply(): List[String] = labels
    new MemberLabelsImpl(labels)

  inline given derived[T](using Mirror.Of[T]): MemberLabels[T] = summonLabels[T]