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

package org.scalawag.bateman.json.focus

import cats.syntax.either._
import org.scalawag.bateman.json._
import org.scalawag.bateman.json.focus.weak._

/** Represents a collection of foci into a single JSON document.
  *
  * This class should not be constructed explicitly. It should only be created as the result of applying a
  * ListJLens to a JFocus.
  */
abstract case class JFoci[+A <: JAny] private[json] (foci: List[JFocus[A]]) {
  // All the foci must have the same root document or else none of the ops methods will work.
  require(foci.map(_.root).distinct.size < 2)
}

object JFoci {
  private[json] def apply[A <: JAny](foci: List[JFocus[A]]) = new JFoci(foci) {}

  implicit class JFociOps[A <: JAny](me: JFoci[A]) {
    def modify[B <: JAny](fn: A => B): JFoci[B] = {
      // Go through each of our foci (in reverse order to preserve indices) and do the replacement.
      // The output of the prior feeds into the input of the next one, so that we get a single
      // document with all the modifications.
      val newRoot =
        me.foci
          .foldRight(None: Option[JFocus[JAny]]) { (f, accOpt) =>
            // First time through, we just use the focus. From then on, we replicate the path in the last root.
            val replicated = accOpt.map(acc => f.replicate(acc.root.value)).getOrElse(f).asInstanceOf[JFocus[A]]
            val mod = replicated.modifyValue(fn)
            Some(mod)
          }
          .map(_.root.value)

      // Now, replicate all of our foci against this new root.
      newRoot match {
        case Some(r) => JFoci(me.foci.map(f => f.replicate(r)))
        case None    => me
      }
    }.asInstanceOf[JFoci[B]] // We know this is true because of the way we constructed the new document.

    def delete(): JResult[JFoci[JAny]] = {
      // Go through each of our foci (in reverse order to preserve indices) and do the deletions to get
      // the new JSON document. The output of the prior deletion feeds into the input of the next one,
      // so that we get a single document with all the deletions.
      val newRoot =
        me.foci
          .foldRight(None.rightNec: JResult[Option[JFocus[JAny]]]) { (f, acc) =>
            acc.flatMap { lastFocus =>
              // First time through, we just use the focus. From then on, we replicate the path in the last root.
              val replicated = lastFocus.map(acc => f.replicate(acc.root.value)).getOrElse(f)
              val modified = replicated.delete()
              modified.map(Some.apply)
            }
          }
          .map(_.map(_.root.value))

      // Now, replicate the parents of all our foci against this new root.
      // TODO: would it be better for any reason to keep track of the focus paths as they come back in the fold above?
      newRoot map {
        // If we made it here, we know that all the foci were children. A root focus would have triggered a failure.
        case Some(r) => JFoci(me.foci.map(_.asInstanceOf[JChildFocus[_, JFocus[JAny]]].parent).distinct.map(f => f.replicate(r)))
        case None    => me
      }
    }

    def values: List[A] = me.foci.map(_.value)

    def root: Option[JFocus[JAny]] = me.foci.headOption.map(_.root)
  }
}
