# fields(String)

The `fields` lens attempts to navigate down to all specified fields of the 
currently focused JSON value. It will fail if the focused value is not a JSON
object. If the focused object has no matching fields, an empty list is 
returned. 

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
  
f(fields("b")).map(_.foci)
```

To streamline your lenses even more, you can use the `**` or `all` modifier 
on a bare string as an alias for the `fields` lens. Of course, this will only
work in places where the compiler is already expecting a lens.

```scala mdoc:bateman:right:list:focus
f("b".**).map(_.foci)
```

```scala mdoc:bateman:right:list:focus
f("b".all).map(_.foci)
```
