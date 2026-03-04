# Annotations

JSON:API resource objects have a specific structure that differs from plain
JSON objects. Annotations tell the derivation machinery which role each
case class field plays in the resource object.

All annotations live in `org.scalawag.bateman.jsonapi.generic.Annotations`.

## @Id

Marks the resource identifier field. The value must be encodable and
decodable as a JSON string (the JSON:API specification requires string IDs).

```scala
case class Person(@Id id: String, ...)
```

If the ID field is `Option[String]`, it is treated as optional -- useful
for resources that haven't been persisted yet.

## @Attribute

Marks a field as a resource attribute. During encoding, the field is placed
inside the `attributes` object. During decoding, it is read from there.

```scala
case class Person(@Id id: String, @Attribute name: String, @Attribute age: Int)
```

Attribute fields can be optional (`Option[A]`) to represent fields that may
be absent in the JSON.

## @Meta

Marks a field as resource metadata. Works the same way as `@Attribute` but
targets the `meta` object instead.

```scala
case class Person(@Id id: String, @Attribute name: String, @Meta createdAt: String)
```

## @Relationship

Marks a field as a relationship reference. During decoding, the field is
read from the `relationships` object's `data` member. The field type
determines the cardinality:

 * `A` -- a required to-one relationship
 * `Nullable[A]` -- a nullable to-one relationship
 * `List[A]` -- a to-many relationship

The relationship target type `A` must have its own decoder/encoder.

```scala
case class Article(
    @Id id: String,
    @Attribute title: String,
    @Relationship author: Person
)
```

## @IncludedRelationship

Like `@Relationship`, but indicates that the related resource should be
fully included (in the `included` array) rather than referenced by ID alone.
During decoding, the related resource is resolved from the `included`
section of the document. During encoding, the full resource is encoded into
the `included` array.

```scala
case class Article(
    @Id id: String,
    @Attribute title: String,
    @IncludedRelationship author: Person
)
```

The same cardinality rules apply as for `@Relationship`.

## @Type

Marks a field that receives the resource type string. This is rarely needed
-- the resource type is normally derived from the class name automatically.
Use this only if your domain model needs to inspect the type value at
runtime.

```scala
case class GenericResource(@Type resourceType: String, @Id id: String)
```
