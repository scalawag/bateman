# items

The `items` lens attempts to navigate down to all item values of the
currently focused JSON value. If the focused array has no items, an empty
list is returned.

This lens has `List` @ref:[cardinality](../cardinality.md).

```scala mdoc:bateman:right:list:focus
import org.scalawag.bateman.json.lens._
import org.scalawag.bateman.json.literal._

val f = jsona"""
  [
    "a",
    4,
    false
  ]
""".asRootFocus
  
f(items).map(_.foci)
```

To streamline your lenses even more, you can use `*` as an alias for the
`items` lens.

```scala mdoc:bateman:right:list:focus
f(*).map(_.foci)
```
