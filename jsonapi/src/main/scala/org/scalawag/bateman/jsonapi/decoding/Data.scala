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

package org.scalawag.bateman.jsonapi.decoding

import cats.data.NonEmptyChain
import cats.syntax.validated._
import cats.syntax.traverse._
import org.scalawag.bateman.json.{NotNull, Null, Nullable}
import org.scalawag.bateman.json.syntax._
import org.scalawag.bateman.json.decoding.{
  DecodeResult,
  Decoder,
  JAny,
  JArray,
  JNull,
  JObject,
  JsonTypeMismatch,
  UnspecifiedField
}
import org.scalawag.bateman.json.generic.decoding.JSource
import org.scalawag.bateman.jsonapi.encoding

trait Data[+A] {
  val src: JAny

  def required: DecodeResult[A]
  def nullable: DecodeResult[Nullable[A]]
  def multiple: DecodeResult[List[A]]

  def toList: List[A]
}

case class NullData(src: JNull) extends Data[Nothing] with PrimaryData with RelationshipData {
  override def required: DecodeResult[Nothing] = JsonTypeMismatch(src, NonEmptyChain(JObject)).invalidNec
  override def nullable: DecodeResult[Nullable[Nothing]] = Null(src).validNec
  override def multiple: DecodeResult[List[Nothing]] = JsonTypeMismatch(src, NonEmptyChain(JObject)).invalidNec
  override def toList: List[Nothing] = Nil
  override def toEncoding: encoding.NullData = encoding.NullData
}

object NullData {
  implicit def decoder: Decoder[JNull, NullData] =
    Decoder { in =>
      NullData(in).validNec
    }
}

trait SingularData[A] extends Data[A] {
  val data: A

  override def required: DecodeResult[A] = data.validNec
  override def nullable: DecodeResult[Nullable[A]] = NotNull(data).validNec
  override def multiple: DecodeResult[List[A]] = JsonTypeMismatch(src, NonEmptyChain(JArray)).invalidNec

  override def toList: List[A] = List(data)
}

trait PluralData[A] extends Data[A] {
  val data: List[A]

  override def required: DecodeResult[A] = JsonTypeMismatch(src, NonEmptyChain(JObject)).invalidNec
  override def nullable: DecodeResult[Nullable[A]] = JsonTypeMismatch(src, NonEmptyChain(JNull, JObject)).invalidNec
  override def multiple: DecodeResult[List[A]] = data.validNec

  override def toList: List[A] = data
}

sealed trait PrimaryData extends Data[ResourceLike] {
  def toEncoding: encoding.PrimaryData
}

object PrimaryData {
  implicit val decoder: Decoder[JAny, PrimaryData] =
    // We have to try to guess at what these will be considered by the decoding developer. Get as close as we can,
    // but there may be some transcoding going on later anyway. Use the most restrictive possible interpretation.
    Decoder.fromPF {
      case jarray: JArray =>
        // This roughly translates to: "If any of the objects in the array have any of the keys forbidden for
        // resource identifiers, decode them as ResourceObjects. Otherwise, decode them as ResourceIdentifiers."
        jarray.items.traverse(_.asObject) andThen { jobjects =>
          if (jobjects.exists(_.fieldList.exists(f => ResourceIdentifier.forbiddenFields.contains(f.name.value))))
            jarray.as[ResourceObjectsData]
          else
            jarray.as[ResourceIdentifiersData]
        }

      case jobject: JObject =>
        // This roughly translates to: "If the object doesn't have an ID, decode it as a ResourceObjectOptionalId.
        // Otherwise, if it has any of the keys forbidden for resource identifiers, decode it as a ResourceObject.
        // Otherwise, decode it as a ResourceIdentifier."
        jobject.fields andThen { ff =>
          if (!ff.contains("id"))
            Decoder[JAny, ResourceObjectOptionalIdData].decode(jobject)
          else if (ff.keySet.intersect(ResourceIdentifier.forbiddenFields).nonEmpty)
            Decoder[JAny, ResourceObjectData].decode(jobject)
          else
            Decoder[JAny, ResourceIdentifierData].decode(jobject)
        }

      case jnull: JNull =>
        // It doesn't really matter which we use, since it's empty. Still, I'm using the strictest thing possible
        // (resource identifier).
        NullData(jnull).validNec
    }
}

sealed trait RelationshipData extends Data[ResourceIdentifier] {
  def toEncoding: encoding.RelationshipData
}

object RelationshipData {
  implicit val decoder: Decoder[JAny, RelationshipData] =
    Decoder { in =>
      in.forType {
        case JArray  => in.as[ResourceIdentifiersData]
        case JObject => in.as[ResourceIdentifierData]
        case JNull   => in.as[NullData]
      }
    }
}

case class ResourceIdentifierData(src: JAny, data: ResourceIdentifier)
    extends SingularData[ResourceIdentifier]
    with PrimaryData
    with RelationshipData {
  override def toEncoding: encoding.ResourceIdentifierData =
    encoding.ResourceIdentifierData(data.toEncoding)
}

object ResourceIdentifierData {
  implicit def decoder: Decoder[JAny, ResourceIdentifierData] =
    Decoder { in =>
      in.as[ResourceIdentifier].map(ResourceIdentifierData(in, _))
    }
}

case class ResourceObjectData(src: JAny, data: ResourceObject) extends SingularData[ResourceObject] with PrimaryData {
  override def toEncoding: encoding.ResourceObjectData =
    encoding.ResourceObjectData(data.toEncoding)

}

object ResourceObjectData {
  implicit def decoder: Decoder[JAny, ResourceObjectData] =
    Decoder { in =>
      in.as[ResourceObject].map(ResourceObjectData(in, _))
    }
}

case class ResourceObjectOptionalIdData(src: JAny, data: ResourceObjectOptionalId)
    extends SingularData[ResourceObjectOptionalId]
    with PrimaryData {
  override def toEncoding: encoding.ResourceObjectOptionalIdData =
    encoding.ResourceObjectOptionalIdData(data.toEncoding)
}

object ResourceObjectOptionalIdData {
  implicit def decoder: Decoder[JAny, ResourceObjectOptionalIdData] =
    Decoder { in =>
      in.as[ResourceObjectOptionalId].map(ResourceObjectOptionalIdData(in, _))
    }
}

case class ResourceIdentifiersData(src: JArray, data: List[ResourceIdentifier])
    extends PluralData[ResourceIdentifier]
    with PrimaryData
    with RelationshipData {
  override def toEncoding: encoding.ResourceIdentifiersData =
    encoding.ResourceIdentifiersData(data.map(_.toEncoding))
}

object ResourceIdentifiersData {
  implicit def decoder: Decoder[JArray, ResourceIdentifiersData] =
    Decoder { in =>
      in.as[List[ResourceIdentifier]].map(ResourceIdentifiersData(in, _))
    }
}

case class ResourceObjectsData(src: JArray, data: List[ResourceObject])
    extends PluralData[ResourceObject]
    with PrimaryData {
  override def toEncoding: encoding.ResourceObjectsData =
    encoding.ResourceObjectsData(data.map(_.toEncoding))
}

object ResourceObjectsData {
  implicit def decoder: Decoder[JArray, ResourceObjectsData] =
    Decoder { in =>
      in.as[List[ResourceObject]].map(ResourceObjectsData(in, _))
    }
}

trait HasData[A] {
  val src: JSource
  val data: Option[A]

  def requiredData: DecodeResult[A] =
    data.map(_.validNec).getOrElse(UnspecifiedField(src.root, "data").invalidNec)
}
