
# Missing Values

There are a few kinds of "missing" in bateman with respect to derived codecs.

- possibly missing in the domain model (@ref:[nullable](#nullable))
- possibly missing in the JSON encoding (@ref:[optional](#optional))
- possibly implied in the JSON encoding  (@ref:[implied](#implied))

These concepts are orthogonal, as you'll see below. The domain may have a 
value that can be missing even though it _must_ be represented in the JSON 
nonetheless through `null`. Independently, some values can be missing from 
incoming and outgoing JSON documents and yet their domain values _may_ be 
implied or not.

This probably sounds confusing at first, but it allows bateman to represent
[patch-style](https://tools.ietf.org/html/rfc7386) documents where not 
specifying a field means to leave it alone specifying a value of `null` 
means to delete its value.

Read on.


## Nullable

When a value may be specified as missing in your _domain model_, but 
that absence should be reflected in the JSON document, the field should be
@scaladoc:[Nullable](org.scalawag.bateman.json.Nullable). This is 
basically a statically-distinct version of `Option` with its concrete members 
called `Null` and `NotNull` instead of `None` and `Some`, respectively. 

The field's absence is represented in JSON through a `null` value and its 
presence through any other JSON value.

```scala mdoc:bateman:list:jany
import org.scalawag.bateman.json._
import org.scalawag.bateman.json.syntax._

final case class MyNullable(a: Nullable[Int])

object MyNullable {
  import org.scalawag.bateman.json.generic.semiauto.unchecked._
  implicit val codec: JObjectCodec[MyNullable] = deriveCodecForCaseClass()
}

List(
  MyNullable(Null).toJAny,
  MyNullable(NotNull(7)).toJAny
)
```

When decoding, bateman will intepret JSON `null` as scala `Null` and 
anything else as `NotNull` containing an appropriate type.

```scala mdoc:bateman:right:list:value
import cats.syntax.traverse._
import org.scalawag.bateman.json.literal._

List(
  json"""{"a": null}""",
  json"""{"a": 17}"""
).traverse(_.asRootFocus.decode[MyNullable])
```

Anything but `null` or the correct JSON type will result in an error.

```scala mdoc:bateman:left
json"""{"a": true}""".asRootFocus.decode[MyNullable]
```

## Optional

When a value may be missing from your _JSON model_, and your domain 
model needs to convey that, the field should be an 
@scaladoc:[Option](scala.Option) in your case class. Optional fields that 
are set to `None` are _never_ included in the encoded JSON. Whether a `Some` 
is included depends on what it contains -- you can use the strategies on this
page simultaneously (e.g., an `Option[Nullable[Int]]` field).

```scala mdoc:bateman:list:jany
final case class MyOptional(a: Option[Int])

object MyOptional {
  import org.scalawag.bateman.json.generic.semiauto.unchecked._
  implicit val codec: JObjectCodec[MyOptional] = deriveCodecForCaseClass()
}

List(
  MyOptional(None).toJAny,
  MyOptional(Some(7)).toJAny
)
```

When decoding, an optional field can _always_ be omitted, in which case it
will be set to `None` in the decoded case class instance.

```scala mdoc:bateman:right:list:value
List(
  json"""{}""",
  json"""{"a": 17}"""
).traverse(_.asRootFocus.decode[MyOptional])
```

## Implied

In addition to the missing cases above, there's another concept that's 
closely related, which is that of _implied_ values. Implied values are those 
that _actually have a value_ but are not represented in the JSON because 
they are set to their default value, as determined by the case class.

Unlike the missing cases, this is not a per-field capability. Each codec must 
be configured to include or omit fields with their default values at the 
time it is derived. The behavior is independent for encoding (through
@ref:[encodeDefaultValues](config/encodeDefaultValues.md)) and
decoding (through
@ref:[useDefaultsForMissingFields](config/useDefaultsForMissingFields.md)),
but it must be the same for all fields in a given codec.
Most commonly, all of your codecs will use the same configuration and, 
therefore the same implication strategies.

When encoding, any fields that are set to their default value will be 
excluded, even if they have been explicitly set in Scala.

```scala mdoc:bateman:list:jany
final case class MyDefault(a: Int = 0)

object MyDefault {
  import org.scalawag.bateman.json.generic.semiauto.unchecked._
  implicit val codec: JObjectCodec[MyDefault] = deriveCodecForCaseClass()
}

List(
  MyDefault().toJAny,
  MyDefault(0).toJAny,
  MyDefault(42).toJAny
)
```

When decoding, this means that unspecified fields will be set to their default 
values.

```scala mdoc:bateman:right:list:value
List(
  json"""{}""",
  json"""{"a": 0}""",
  json"""{"a": 42}"""
).traverse(_.asRootFocus.decode[MyDefault])
```

@@@ warning
The default values of optional (`Option`) fields are completely ignored by the 
codecs. This means that, even if your case class has a `Some` default value,
the decoded case class instance will have a `None` for an absent field.

You can still specify defaults where they're convenient for the consumers of 
your Scala API. Just understand that the codecs won't use them.
@@@

# Combining Styles

These strategies can be mixed together. As an example, nesting optional 
and nullable allows you to distinguish between incoming JSON that _contains_ 
the `null` value of a field and incoming JSON that _omits_ the field entirely.
This is particularly useful in patch-style documents.

```scala mdoc:bateman:list:jany
final case class MyOptNul(a: Option[Nullable[Int]])

object MyOptNul {
  import org.scalawag.bateman.json.generic.semiauto.unchecked._
  implicit val codec: JObjectCodec[MyOptNul] = deriveCodecForCaseClass()
}

List(
  MyOptNul(None).toJAny,
  MyOptNul(Some(Null)).toJAny,
  MyOptNul(Some(NotNull(7))).toJAny
)
```

On decoding, both `null` and a number can be conveyed through the decoded
case class instance.

```scala mdoc:bateman:right:list:value
import cats.syntax.traverse._
import org.scalawag.bateman.json.literal._

List(
  json"""{}""",
  json"""{"a": null}""",
  json"""{"a": 17}"""
).traverse(_.asRootFocus.decode[MyOptNul])
```

