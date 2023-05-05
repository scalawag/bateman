# Decoding

To support easily decoding JSON:API, bateman provides a set of
@scaladoc:[lenses](org.scalawag.bateman.jsonapi.lens.index) that are specific
to the JSON:API specification. You can use them and compose them the same way
that you use @ref:[lenses](../json/lens/index.md) for JSON documents.

There's a corresponding lens for practically every reserved field name in the 
JSON:API specification. You can mix them with the JSON lenses to build deep 
lenses.

```scala mdoc:bateman:right
import org.scalawag.bateman.json.literal._
import org.scalawag.bateman.json.lens._
import org.scalawag.bateman.jsonapi.lens._

val input = json"""
  {
    "data": {
      "type": "person",
      "id": "185",
      "attributes": {
        "name": "Joe",
        "birthday": "1997-06-16",
        "address": {
          "street1": "123 Main St.",
          "street2": null,
          "city": "Austin",
          "state": "TX",
          "zip": "78758"
        }
      },
      "relationships": {
        "manager": {
          "data": {
            "type": "person",
            "id": "41"
          }
        }
      },
      "meta": {
        "created_at": "2023-05-04T14:35:16Z"
      }
    },
    "included": [
      {
        "type": "person",
        "id": "41",
        "attributes": {
          "name": "Tom",
          "birthday": "1984-04-23",
          "address": {
            "street1": "3313 Maple Ave.",
            "street2": "Apt. 2B",
            "city": "New York",
            "state": "NY",
            "zip": "10101"
          }
        },
        "meta": {
          "created_at": "2021-11-17T07:40:03Z"
        }
      }
    ]
  }
""".asRootFocus

input(data ~> attributes ~> "address" ~> "city")
```

Additionally, there are three lens `def`s representing the named `meta`,
`attributes` and `relationships` of a
[resource](https://jsonapi.org/format/#document-resource-objects) to make 
the intent a little bit clearer.

```scala mdoc:bateman:right
import java.time.Instant
import org.scalawag.bateman.json.focus.weak._

input(data ~> meta("created_at")).flatMap(_.decode[Instant])
```

There is, one more lens that will conveniently dereference 
a resource identifier for you and return the associated included resource 
object.

```scala mdoc:bateman:right:focus
import java.time.Instant
import org.scalawag.bateman.json.focus.weak._

input(data ~> relationship("manager") ~> data ~> includedRef ~> attribute("address") ~> "zip")
```

You'll probably normally use case classes to represent your domain model and
use derived codecs (TODO link) to extract most of the data from your
JSON:API documents. Occasionally, though, when you just want to extract a
specific piece of metadata (that's not part of your model) or extract a
single ID from deep within the document (without having to decode everything),
you may to use these lenses for a more ad hoc way of querying the document. 
