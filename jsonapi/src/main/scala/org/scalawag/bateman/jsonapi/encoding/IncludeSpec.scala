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

package org.scalawag.bateman.jsonapi.encoding

import cats.syntax.either._
import org.scalawag.bateman.json.rightIfEmpty

/** Used to track the inclusion of related resources. This represents a node in the tree containing all the
  * include paths.
  *
  * https://jsonapi.org/format/#fetching-includes
  */

sealed trait IncludeSpec {

  /** Gets the explicit child nodes of this node. */
  def explicitChildren: Set[String]

  /** Gets the IncludeSpec for the named relationship from this resource. */
  def descend(child: String): IncludeSpec
}

/** An include specification that can't cause a failure because it doesn't explicitly mention any paths,
  * avoiding the possibility of a nonexistent or unavailable path.
  */
sealed trait InfallibleIncludeSpec extends IncludeSpec {
  override def explicitChildren: Set[String] = Set.empty
}

object IncludeSpec {

  /** This path should _always_ be included. If it's not available at the time of encoding the referring resource,
    * the encoding fails. Children must be explicitly requested or else they are excluded.
    *
    * This class represents paths that are explicitly specified by the HTTP client as part of a query parameter,
    * though it can dome from anywhere in the code, or implied by an extension of a path. For example, the string
    * "a.b.c" is explicitly specified and the parent path "a.b" is implied.
    *
    * @param path contains the path from the root to here (used for reporting errors)
    * @param childSpecs child nodes in the tree
    */
  case class Always(path: String, childSpecs: Map[String, IncludeSpec]) extends IncludeSpec {
    override def explicitChildren: Set[String] = childSpecs.keySet
    override def descend(child: String): IncludeSpec = childSpecs.getOrElse(child, Never)
  }

  /** This path should _never_ be included. Even if the resource object is already available, the caller has
    * requested that it not be included. Any paths that are extensions of this should also never be included.
    *
    * This class represents paths that are ''not'' explicitly specified by the HTTP client as part of a query
    * parameter. When the code queries an include path that was ''not'' specified, this is what is returned.
    */
  case object Never extends InfallibleIncludeSpec {
    override def descend(child: String): IncludeSpec = Never
  }

  /** This path should be included whenever it is already available as part of its referring resource. The inability
    * to include the resource object is not considered a cause for failure. Any paths that are extensions of this
    * should also be included opportunistically.
    *
    * This class is never used to represent the query parameter specified by the HTTP client. It can only be
    * specified within the code to indicate that everything available should be encoded.
    */
  case object Opportunistically extends InfallibleIncludeSpec {
    override def descend(child: String): IncludeSpec = Opportunistically
  }

  /** Turns an include spec (parameter) into an IncludeSpec (tree). */

  def apply(spec: String, lengthLimit: Int = 1024, depthLimit: Int = 8): EncodeResult[IncludeSpec] = {
    def go(prefix: List[String], paths: Array[Array[String]]): IncludeSpec = {
      val children =
        paths.groupBy(_.head).mapValues(_.map(_.tail)).toMap map {
          case (h, pp) => h -> go(prefix :+ h, pp.filterNot(_.isEmpty))
        }
      Always(prefix.mkString("."), children)
    }

    if (spec.isEmpty)
      Never.rightNec
    else if (spec.length > lengthLimit)
      IncludeTooLong(lengthLimit).leftNec
    else {
      val paths = spec.split(',').map(_.split('.'))
      val errors = paths.filter(_.length > depthLimit).map(p => IncludePathTooDeep(p.mkString("."), depthLimit))
      rightIfEmpty(errors, go(Nil, paths))
    }
  }

  /** The same as `apply` but throws any errors. */
  def unsafe(spec: String, lengthLimit: Int = 1024, depthLimit: Int = 8): IncludeSpec =
    apply(spec, lengthLimit, depthLimit).fold(
      ee => throw new IllegalArgumentException(ee.map(_.toJsonApiError.detail).iterator.mkString("\n")),
      identity
    )

}
