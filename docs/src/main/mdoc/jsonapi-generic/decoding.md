# Decoding

@@@ note
The examples on this page use the following preface.
```scala mdoc:silent
import org.scalawag.bateman.json._
import org.scalawag.bateman.json.literal._
import org.scalawag.bateman.json.lens._
import org.scalawag.bateman.jsonapi.generic.Annotations._
import org.scalawag.bateman.jsonapi.lens._
```
@@@

Decoding turns JSON:API resource documents into your annotated case classes.
The derived `JObjectDecoder` knows how to read each field from the correct
part of the resource object based on its @ref:[annotation](annotations.md).

## Automatic Decoding

Import `auto._` and decode a resource object from a focus.

```scala mdoc:bateman:right:value
import org.scalawag.bateman.jsonapi.generic.auto._

case class Widget(@Id id: String, @Attribute color: String, @Attribute weight: Int)

val doc = json"""
  {
    "data": {
      "type": "Widget",
      "id": "42",
      "attributes": {
        "color": "blue",
        "weight": 150
      }
    }
  }
""".asRootFocus

doc(data ~> narrowTo[JObject]).flatMap(_.decode[Widget])
```

The decoder validates that the resource type matches the expected type
(derived from the class name) and extracts each field from its proper
location.

```scala mdoc:reset:silent
import org.scalawag.bateman.json._
import org.scalawag.bateman.json.literal._
import org.scalawag.bateman.json.lens._
import org.scalawag.bateman.jsonapi.generic.Annotations._
import org.scalawag.bateman.jsonapi.lens._
```

## Semiautomatic Decoding

Use `semiauto` to explicitly derive decoders with more control.

```scala mdoc:bateman:right:value
import org.scalawag.bateman.jsonapi.generic.semiauto._

case class Gadget(@Id id: String, @Attribute name: String, @Meta version: Int)

object Gadget {
  implicit val decoder: JObjectDecoder[Gadget] =
    deriveResourceDecoderForCaseClass[Gadget]()
}

val gadgetDoc = json"""
  {
    "data": {
      "type": "Gadget",
      "id": "7",
      "attributes": {
        "name": "gizmo"
      },
      "meta": {
        "version": 3
      }
    }
  }
""".asRootFocus

gadgetDoc(data ~> narrowTo[JObject]).flatMap(_.decode[Gadget])
```

### Overriding the Resource Type

You can override the expected resource type name so that it doesn't have to
match the class name.

```scala mdoc:bateman:right:value
import org.scalawag.bateman.jsonapi.generic.semiauto._

case class ServerNode(@Id id: String, @Attribute hostname: String)

implicit val serverNodeDecoder: JObjectDecoder[ServerNode] =
  deriveResourceDecoderForCaseClass[ServerNode](resourceType = "nodes")

val nodeDoc = json"""
  {
    "data": {
      "type": "nodes",
      "id": "1",
      "attributes": {
        "hostname": "web-01"
      }
    }
  }
""".asRootFocus

nodeDoc(data ~> narrowTo[JObject]).flatMap(_.decode[ServerNode])
```

```scala mdoc:reset:silent
import org.scalawag.bateman.json._
import org.scalawag.bateman.json.literal._
import org.scalawag.bateman.json.lens._
import org.scalawag.bateman.jsonapi.generic.Annotations._
import org.scalawag.bateman.jsonapi.lens._
```

## Field Name Mapping

Use @ref:[`Config`](../json-generic/config/index.md) to control how field
names are mapped from JSON to case class fields. This affects attribute,
meta, and relationship names.

```scala mdoc:bateman:right:value
import org.scalawag.bateman.jsonapi.generic.auto._
import org.scalawag.bateman.json.generic.Config
import org.scalawag.bateman.json.generic.naming._

case class UserProfile(@Id id: String, @Attribute firstName: String, @Attribute lastName: String)

implicit val config: Config = Config(fieldNameMapping = CamelCase to SnakeCase)

val profileDoc = json"""
  {
    "data": {
      "type": "UserProfile",
      "id": "1",
      "attributes": {
        "first_name": "Jane",
        "last_name": "Doe"
      }
    }
  }
""".asRootFocus

profileDoc(data ~> narrowTo[JObject]).flatMap(_.decode[UserProfile])
```

```scala mdoc:reset:silent
import org.scalawag.bateman.json._
import org.scalawag.bateman.json.literal._
import org.scalawag.bateman.json.lens._
import org.scalawag.bateman.jsonapi.generic.Annotations._
import org.scalawag.bateman.jsonapi.lens._
```

## Decoding Relationships

Fields annotated with `@Relationship` are decoded from the `relationships`
object. The relationship target type must have its own decoder.

```scala mdoc:bateman:right:value
import org.scalawag.bateman.jsonapi.generic.auto._

case class AuthorRef(@Id id: String)
case class Article(@Id id: String, @Attribute title: String, @Relationship author: AuthorRef)

val articleDoc = json"""
  {
    "data": {
      "type": "Article",
      "id": "1",
      "attributes": {
        "title": "On Monads"
      },
      "relationships": {
        "author": {
          "data": {
            "type": "AuthorRef",
            "id": "42"
          }
        }
      }
    }
  }
""".asRootFocus

articleDoc(data ~> narrowTo[JObject]).flatMap(_.decode[Article])
```

## Decoding Included Relationships

Fields annotated with `@IncludedRelationship` resolve the relationship
reference against the `included` section of the document to produce the
full resource.

```scala mdoc:bateman:right:value
import org.scalawag.bateman.jsonapi.generic.auto._

case class FullAuthor(@Id id: String, @Attribute name: String)
case class Essay(@Id id: String, @Attribute title: String, @IncludedRelationship author: FullAuthor)

val essayDoc = json"""
  {
    "data": {
      "type": "Essay",
      "id": "1",
      "attributes": {
        "title": "On Functors"
      },
      "relationships": {
        "author": {
          "data": {
            "type": "FullAuthor",
            "id": "42"
          }
        }
      }
    },
    "included": [
      {
        "type": "FullAuthor",
        "id": "42",
        "attributes": {
          "name": "Phil"
        }
      }
    ]
  }
""".asRootFocus

essayDoc(data ~> narrowTo[JObject]).flatMap(_.decode[Essay])
```

```scala mdoc:reset:silent
import org.scalawag.bateman.json._
import org.scalawag.bateman.json.literal._
import org.scalawag.bateman.json.lens._
import org.scalawag.bateman.jsonapi.generic.Annotations._
import org.scalawag.bateman.jsonapi.lens._
```

## Decoding Sealed Traits

Sealed traits decode using the resource `type` field as a discriminator.
Each concrete subtype must have its own decoder, and the trait decoder
dispatches based on the discriminator value.

```scala mdoc:bateman:right:value
import org.scalawag.bateman.jsonapi.generic.semiauto._

sealed trait Shape
case class Circle(@Id id: String, @Attribute radius: Int) extends Shape
case class Rect(@Id id: String, @Attribute width: Int, @Attribute height: Int) extends Shape

object Circle {
  implicit val decoder: JObjectDecoder[Circle] = deriveResourceDecoderForCaseClass[Circle]()
}
object Rect {
  implicit val decoder: JObjectDecoder[Rect] = deriveResourceDecoderForCaseClass[Rect]()
}
implicit val shapeDecoder: JObjectDecoder[Shape] = deriveResourceDecoderForTrait[Shape]()

val shapeDoc = json"""
  {
    "data": {
      "type": "Circle",
      "id": "1",
      "attributes": {
        "radius": 5
      }
    }
  }
""".asRootFocus

shapeDoc(data ~> narrowTo[JObject]).flatMap(_.decode[Shape])
```

Custom @ref:[discriminator](../json-generic/discriminators.md) values and
fields work the same way as for plain JSON generic derivation.
