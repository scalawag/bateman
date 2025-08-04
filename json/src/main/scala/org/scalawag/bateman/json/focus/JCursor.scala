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

package org.scalawag.bateman.json.focus

import cats.{Eval, Foldable, Traverse}
import cats.syntax.either._
import cats.syntax.functor._
import cats.syntax.foldable._
import cats.syntax.parallel._
import org.scalawag.bateman.json._

/** Represents a collection of foci into a single JSON document.
  *
  * This class should not be constructed explicitly. It should only be created as the result of applying a
  * ListJLens to a JFocus.
  */
case class JCursor[F[+_]: Traverse, +A <: JAny](foci: F[JFocus[A]]) {
  // All the foci must have the same root document or else none of the ops methods will work.
  require(foci.map(_.root).toList.distinct.size < 2)
}

object JCursor {
  implicit def cursorToFoci[F[+_]: Traverse, A <: JAny](in: JCursor[F, A]): F[JFocus[A]] = in.foci

  implicit class JCursorOps[F[+_]: Traverse: Foldable: JCursor.Distinct, A <: JAny](me: JCursor[F, A]) {
    def modify[B <: JAny](fn: JFocus[A] => B): JCursor[F, B] = {
      // Go through each of our foci (in reverse order to preserve indices) and do the replacement.
      // The output of the prior feeds into the input of the next one, so that we get a single
      // document with all the modifications. We also track the stripped B values for each focus.
      val (newRootOpt, bValues) =
        me.foci
          .foldr(Eval.now((None: Option[JAny], List.empty[B]))) { (f, accEval) =>
            val (lastRoot, bVals) = accEval.value
            // First time through, we just use the focus. From then on, we replicate the path in the last root.
            val replicated = lastRoot.map(r => new JFocusWeakOps(f).replicateAs(r, f.value)).getOrElse(f)
            val mod = new JFocusWeakOps(replicated).replace(fn(replicated))
            Eval.now((Some(mod.root.value), mod.value :: bVals))
          }
          .value

      // Now, replicate all of our foci against this new root, using the tracked B values.
      newRootOpt match {
        case Some(r) =>
          val bIter = bValues.iterator
          JCursor(me.foci.map(f => new JFocusWeakOps(f).replicateAs(r, bIter.next())))
        case None =>
          JCursor(me.foci.map(f => new JFocusWeakOps(f).replicateAs(f.root.value, fn(f))))
      }
    }

    def delete(): JResult[JCursor[F, JAny]] = {
      // Go through each of our foci (in reverse order to preserve indices) and do the deletions to get
      // the new JSON document. The output of the prior deletion feeds into the input of the next one,
      // so that we get a single document with all the deletions.
      val newRoot =
        me.foci
          .foldr(Eval.now(None.rightNec): Eval[JResult[Option[JFocus[JAny]]]]) { (f, acc) =>
            Eval.now(acc.value.flatMap { lastFocus =>
              // First time through, we just use the focus. From then on, we replicate the path in the last root.
              val replicated = lastFocus.map(acc => f.replicate(acc.root.value)).getOrElse(f)
              val modified = replicated.delete()
              modified.map(Some.apply)
            })
          }
          .value
          .map(_.map(_.root.value))

      // Now, replicate the parents of all our foci against this new root.
      // TODO: would it be better for any reason to keep track of the focus paths as they come back in the fold above?
      newRoot map {
        // If we made it here, we know that all the foci were children. A root focus would have triggered a failure.
        case Some(r) =>
          val parents = me.foci.map {
            case f: JChildFocus[_, _] => f.parent
            case _: JRootFocus[_] => throw new IllegalStateException("unexpected root focus")
          }
          val distinctParents = implicitly[Distinct[F]].distinct(parents)
          JCursor(distinctParents.map(f => f.replicate(r)))
        case None => me
      }
    }

    def decode[B](implicit dec: Decoder[A, B]): JResult[F[B]] = me.foci.parTraverse(_.decode[B])

    def values: F[A] = me.foci.map(_.value)

    def root: Option[JFocus[JAny]] = me.foci.toList.headOption.map(_.root)
  }

  trait Distinct[F[+_]] {
    def distinct[A](fa: F[A]): F[A]
  }

  object Distinct {
    implicit val distinctForSingle: Distinct[Single] = new Distinct[Single] {
      override def distinct[A](fa: Single[A]): Single[A] = fa
    }

    implicit val distinctForOption: Distinct[Option] = new Distinct[Option] {
      override def distinct[A](fa: Option[A]): Option[A] = fa
    }

    implicit val distinctForList: Distinct[List] = new Distinct[List] {
      override def distinct[A](fa: List[A]): List[A] = fa.distinct
    }
  }
}
