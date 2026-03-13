@@@ index
* [X](annotations.md)
* [X](encoding.md)
* [X](decoding.md)
@@@

# JSON:API Generic

The `bateman-jsonapi-generic` module provides automatic and semiautomatic
derivation of JSON:API resource encoders and decoders for case classes and
sealed traits. It builds on the @ref:[json-generic](../json-generic/index.md)
module's derivation infrastructure and the
@ref:[JSON:API](../jsonapi/index.md) document model.

Where plain JSON generic derivation maps case class fields directly to JSON
object fields, JSON:API generic derivation maps annotated fields to the
correct locations in a
[JSON:API resource object](https://jsonapi.org/format/#document-resource-objects)
-- `id`, `type`, `attributes`, `meta`, and `relationships`.

@@toc { depth=2 }

## Quick Example

```scala mdoc:bateman:jany
import org.scalawag.bateman.json._
import org.scalawag.bateman.jsonapi.generic.Annotations._
import org.scalawag.bateman.jsonapi.generic.auto._
import org.scalawag.bateman.jsonapi.syntax._

case class Book(@Id id: String, @Attribute title: String, @Attribute pages: Int)

Book("1", "The Great Gatsby", 180).toDocument.toJObject
```

The `@Id` annotation marks the resource identifier, and `@Attribute` marks
fields that belong in the `attributes` object. The resource type is derived
from the class name by default.

## Automatic vs. Semiautomatic Derivation

Just like @ref:[json-generic](../json-generic/index.md), this module supports
both automatic and semiautomatic derivation.

**Automatic derivation** requires only an import and works anywhere an
implicit `ResourceEncoder`, `JObjectDecoder`, or `ResourceCodec` is needed.

```scala
import org.scalawag.bateman.jsonapi.generic.auto._
```

**Semiautomatic derivation** lets you explicitly create codecs with more
control over configuration, resource type names, and discriminators.

```scala
import org.scalawag.bateman.jsonapi.generic.semiauto._
```

See the @ref:[encoding](encoding.md) and @ref:[decoding](decoding.md) pages
for detailed examples of each approach.

## Configuration

JSON:API generic derivation reuses the same
@ref:[`Config`](../json-generic/config/index.md) from json-generic. This
controls:

 * @ref:[Field name mapping](../json-generic/config/fieldNameMapping.md) --
   how case class field names are translated to attribute, meta, and
   relationship names
 * @ref:[Class name mapping](../json-generic/config/classNameMapping.md) --
   how class names are translated to resource type names
 * @ref:[Default value encoding](../json-generic/config/encodeDefaultValues.md)
   -- whether to include fields set to their default values
 * @ref:[Missing field defaults](../json-generic/config/useDefaultsForMissingFields.md)
   -- whether to use defaults for missing fields during decoding
 * @ref:[Unknown field handling](../json-generic/config/allowUnknownFields.md)
   -- whether to allow unexpected fields during decoding
