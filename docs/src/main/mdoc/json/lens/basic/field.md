# field

The `field` lens attempts to navigate down to the field of the currently 
focused JSON value. It will fail if the focused value is not a JSON 
object or if there is not exactly one field with the specified name on that 
object.

This lens has `Creatable` @ref:[cardinality](../cardinality.md).

```scala mdoc:bateman:right:focus
import org.scalawag.bateman.json.lens._
import org.scalawag.bateman.json.literal._

val f = json"""
  {
    "a": {
      "b": 4
    }
  }
""".asRootFocus
  
f(field("a"))
```

To streamline your lenses even more, you can use a bare string as an alias 
for the `field` lens. Of course, this will only work in places where the 
compiler is expecting a lens.

```scala mdoc:bateman:right:focus
f("a")
```

If you want to optionally descend to a field, use the `?` modifier.

```scala mdoc:bateman:right:some:focus
f("a".?)
```
```scala mdoc:bateman:right:none
f("c".?)
```
