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
import org.scalawag.bateman.json.validating.{ValidationFailure, ValidationResult}
import shapeless.tag
import shapeless.tag.@@

object ValidTest {
  trait String128

  trait A

  object A {
    def apply(x: Int): ValidationResult[Int with A] =
      if (x > 0)(x.asInstanceOf[Int with A]).validNec
      else
        ValidationFailure("has to be greater than zero").invalidNec
  }

//  val g = tag[A](tag[String128](6))

//  val g: Int with A @@ String128 = A(8).getOrElse(???)

}
