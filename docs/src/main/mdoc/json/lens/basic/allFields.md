# fields

The `fields` lens attempts to navigate down to all field values of the 
currently focused JSON value. If the focused object has no fields, an empty 
list is returned.

This lens has `List` @ref:[cardinality](../cardinality.md).

```scala mdoc:bateman:right:list:focus
import org.scalawag.bateman.json.lens._
import org.scalawag.bateman.json.literal._

val f = json"""
  {
    "a": {
      "b": 4
    },
    "b": 8,
    "c": false,
    "b": "foo"
  }
""".asRootFocus
  
f(fields).map(_.foci)
```

To streamline your lenses even more, you can use `**` as an alias for the
`fields` lens.

```scala mdoc:bateman:right:list:focus
f(**).map(_.foci)
```
