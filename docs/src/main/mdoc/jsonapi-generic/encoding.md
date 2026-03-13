# Encoding

@@@ note
The examples on this page use the following preface.
```scala mdoc:silent
import org.scalawag.bateman.json._
import org.scalawag.bateman.json.syntax._
import org.scalawag.bateman.jsonapi.generic.Annotations._
import org.scalawag.bateman.jsonapi.syntax._
```
@@@

Encoding turns your annotated case classes into JSON:API resource documents.
The derived `ResourceEncoder` knows how to place each field in the correct
part of the resource object based on its @ref:[annotation](annotations.md).

## Automatic Encoding

Import `auto._` and call `toDocument` on any annotated case class.

```scala mdoc:bateman:jany
import org.scalawag.bateman.jsonapi.generic.auto._

case class Widget(@Id id: String, @Attribute color: String, @Attribute weight: Int)

Widget("42", "blue", 150).toDocument.toJObject
```

The resource type defaults to the simple class name (`Widget`). Fields at
their default values are omitted unless
@ref:[configured otherwise](../json-generic/config/encodeDefaultValues.md).

```scala mdoc:reset:silent
import org.scalawag.bateman.json._
import org.scalawag.bateman.json.syntax._
import org.scalawag.bateman.jsonapi.generic.Annotations._
import org.scalawag.bateman.jsonapi.syntax._
```

## Semiautomatic Encoding

Use `semiauto` to explicitly derive encoders with more control.

```scala mdoc:bateman:jany
import org.scalawag.bateman.jsonapi.generic.semiauto._
import org.scalawag.bateman.jsonapi.encoding.ResourceEncoder

case class Gadget(@Id id: String, @Attribute name: String, @Meta version: Int)

object Gadget {
  implicit val encoder: ResourceEncoder[Gadget] =
    deriveResourceEncoderForCaseClass[Gadget]()
}

Gadget("7", "gizmo", 3).toDocument.toJObject
```

### Overriding the Resource Type

By default, the resource type is derived from the class name (optionally
transformed by the
@ref:[class name mapping](../json-generic/config/classNameMapping.md)
configuration). You can override it explicitly.

```scala mdoc:bateman:jany
import org.scalawag.bateman.jsonapi.generic.semiauto._
import org.scalawag.bateman.jsonapi.encoding.ResourceEncoder

case class ServerNode(@Id id: String, @Attribute hostname: String)

implicit val encoder: ResourceEncoder[ServerNode] =
  deriveResourceEncoderForCaseClass[ServerNode](resourceTypeOverride = "nodes")

ServerNode("1", "web-01").toDocument.toJObject
```

```scala mdoc:reset:silent
import org.scalawag.bateman.json._
import org.scalawag.bateman.json.syntax._
import org.scalawag.bateman.jsonapi.generic.Annotations._
import org.scalawag.bateman.jsonapi.syntax._
```

## Field Name Mapping

Use @ref:[`Config`](../json-generic/config/index.md) to control how field
names are transformed. This affects attribute, meta, and relationship names.

```scala mdoc:bateman:jany
import org.scalawag.bateman.jsonapi.generic.auto._
import org.scalawag.bateman.json.generic.Config
import org.scalawag.bateman.json.generic.naming._

case class UserProfile(@Id id: String, @Attribute firstName: String, @Attribute lastName: String)

implicit val config: Config = Config(fieldNameMapping = CamelCase to SnakeCase)

UserProfile("1", "Jane", "Doe").toDocument.toJObject
```

```scala mdoc:reset:silent
import org.scalawag.bateman.json._
import org.scalawag.bateman.json.syntax._
import org.scalawag.bateman.jsonapi.generic.Annotations._
import org.scalawag.bateman.jsonapi.syntax._
```

## Class Name Mapping

The resource type name is derived from the class name. Use `classNameMapping`
to transform it.

```scala mdoc:bateman:jany
import org.scalawag.bateman.jsonapi.generic.auto._
import org.scalawag.bateman.json.generic.Config
import org.scalawag.bateman.json.generic.naming._

case class BlogPost(@Id id: String, @Attribute title: String)

implicit val config: Config = Config(classNameMapping = PascalCase to KebabCase)

BlogPost("1", "Hello World").toDocument.toJObject
```

```scala mdoc:reset:silent
import org.scalawag.bateman.json._
import org.scalawag.bateman.json.syntax._
import org.scalawag.bateman.jsonapi.generic.Annotations._
import org.scalawag.bateman.jsonapi.syntax._
```

## Encoding Relationships

Fields annotated with `@Relationship` encode as relationship references
(resource identifiers). Fields annotated with `@IncludedRelationship` also
place the full resource in the `included` array.

```scala mdoc:bateman:jany
import org.scalawag.bateman.jsonapi.generic.auto._

case class Author(@Id id: String, @Attribute name: String)
case class Post(@Id id: String, @Attribute title: String, @IncludedRelationship author: Author)

Post("1", "On Monads", Author("42", "Phil")).toDocument.toJObject
```

Resources without an `@Id` field (or with an absent optional ID) are
assigned a Local ID (`lid`) automatically using the configured
`LidGenerator`.

```scala mdoc:reset:silent
import org.scalawag.bateman.json._
import org.scalawag.bateman.json.syntax._
import org.scalawag.bateman.jsonapi.generic.Annotations._
import org.scalawag.bateman.jsonapi.syntax._
```

## Encoding Sealed Traits

Sealed traits work with discriminator-based dispatch, just like in
@ref:[json-generic](../json-generic/discriminators.md). By default, the
`type` field in the resource object doubles as the discriminator.

```scala mdoc:bateman:jany
import org.scalawag.bateman.jsonapi.generic.semiauto._
import org.scalawag.bateman.jsonapi.encoding.ResourceEncoder

sealed trait Shape
case class Circle(@Id id: String, @Attribute radius: Int) extends Shape
case class Rect(@Id id: String, @Attribute width: Int, @Attribute height: Int) extends Shape

object Circle {
  implicit val encoder: ResourceEncoder[Circle] = deriveResourceEncoderForCaseClass[Circle]()
}
object Rect {
  implicit val encoder: ResourceEncoder[Rect] = deriveResourceEncoderForCaseClass[Rect]()
}
implicit val shapeEncoder: ResourceEncoder[Shape] = deriveResourceEncoderForTrait[Shape]()

(Circle("1", 5): Shape).toDocument.toJObject
```

Since JSON:API resources already have a `type` field, it naturally serves
as the discriminator in most cases.
