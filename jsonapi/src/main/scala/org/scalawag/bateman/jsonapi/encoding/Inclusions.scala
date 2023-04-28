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

import cats.Monoid
import cats.syntax.parallel._
import org.scalawag.bateman.json._
import org.scalawag.bateman.json.lens._
import org.scalawag.bateman.jsonapi.lens._
import org.scalawag.bateman.json.focus.weak._
import org.scalawag.bateman.json.{JErrors, JObject, ProgrammerError}
import org.scalawag.bateman.jsonapi.encoding.Inclusions.Key
import cats.syntax.either._
import org.scalawag.bateman.json.focus.JFocus

import scala.collection.immutable.TreeMap

/** Serves to manage included resource objects. Will throw if a different object with the same ID is already included
  * because this is a programming error and should never occur if the system is behaving properly. Automatically sorts
  * the included objects by type and then id.
  */
case class Inclusions private (private val objectMap: TreeMap[Key, JObject] = TreeMap.empty) {
  def +(other: JObject): Inclusions = {
    val key = Key(other.asRootFocus)
    val stripped = other.stripLocation
    objectMap.get(key) match {
      case Some(existing) if existing != stripped =>
        throw ProgrammerError(s"inconsistent duplicate resource objects:\n${existing.spaces2}\n${other.spaces2}")
      case Some(_) =>
        this
      case None =>
        Inclusions(objectMap.updated(key, stripped))
    }
  }

  def objects: Iterable[JObject] = objectMap.values
}

object Inclusions {
  val empty: Inclusions = new Inclusions()

  implicit val monoid: Monoid[Inclusions] = new Monoid[Inclusions] {
    override def empty: Inclusions = Inclusions.empty
    override def combine(x: Inclusions, y: Inclusions): Inclusions = y.objects.foldLeft(x)(_ + _)
  }

  def apply(objects: JObject*): Inclusions = objects.foldLeft(Inclusions.empty)(_ + _)

  case class Key(resourceType: String, id: String, local: Boolean) {
    override val toString: String = s"""(type="$resourceType", ${if (local) "lid" else "id"}="${id}")"""
  }

  object Key {
    def apply(in: JFocus[JObject]): Key =
      in.decode[Key].fold(ee => throw ProgrammerError(JErrors.formatErrorReport(ee)), identity)

    implicit val requiredDecoder: Decoder[JObject, Key] =
      in => {
        import org.scalawag.bateman.jsonapi.lens._
        (in(resourceType).decode[String], in(id.?).decode[String], in(lid.?)).parFlatMapN { (rt, cidOpt, clidOpt) =>
          (cidOpt, clidOpt) match {
            case (Some(_), Some(clid)) =>
              // lid is disallowed when id is present.
              UnexpectedValue(clid).leftNec
            case (Some(cid), None) =>
              Key(rt, cid, false).rightNec
            case (None, Some(clid)) =>
              Key(rt, clid.value.value, true).rightNec
            case (None, None) =>
              MissingField(in, "id", "lid").leftNec
          }
        }
      }

    implicit val optionalDecoder: Decoder[JObject, Option[Key]] =
      requiredDecoder.decode(_).map(Option(_)).ignoreMissingValuesWith(None)

    implicit val ordering: Ordering[Key] = Ordering.by(k => k.resourceType -> k.local -> k.id)
  }

  private def keyedPrimaryData(root: JFocus[JAny]): JResult[List[(Key, JFocus[JObject])]] = {
    // Grab all of the resource objects from the primary data, regardless of whether they have keys.
    val allData =
      root(data.?)
        .flatMap {
          case Some(f @ JFocus.Value(_: JNull))   => Nil.rightNec
          case Some(f @ JFocus.Value(_: JArray))  => f(* ~> narrow[JObject]).map(_.foci)
          case Some(f @ JFocus.Value(o: JObject)) => f.asObject.map(List(_))
          case Some(f)                            => JsonTypeMismatch(f, JObject, JNull, JArray).leftNec
          case None                               => Nil.rightNec
        }

    // Filter it down to the ones that have IncludeKeys and put them into a map.
    allData
      .flatMap { fos =>
        fos.parTraverse { fo =>
          fo.decode[Option[Key]].map { keyOption =>
            keyOption.map(_ -> fo)
          }
        }
      }
      .map(_.flatten)
  }

  private def keyedIncluded(root: JFocus[JAny]): JResult[List[(Key, JFocus[JObject])]] =
    root(included.? ~> * ~> narrow[JObject])
      .map(_.foci)
      .flatMap { fos =>
        fos.parTraverse { fo =>
          fo.decode[Key].map(_ -> fo)
        }
      }

  def findResourceObjects(root: JFocus[JAny]): JResult[Map[Key, List[JFocus[JObject]]]] =
    (
      keyedPrimaryData(root),
      keyedIncluded(root)
    ).parMapN(_ ::: _).map(_.groupBy(_._1).mapValues(_.map(_._2)).toMap)
}
