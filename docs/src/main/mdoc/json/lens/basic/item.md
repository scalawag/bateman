# item(Int)

The item lens attempts to navigate down to the item of the currently
focused JSON value. It will fail if the focused value is not a JSON
array or if the specified index does not exist in the array (i.e., if it's 
out of bounds).

This lens has `Id` @ref:[cardinality](../cardinality.md).

```scala mdoc:bateman:right:focus
import org.scalawag.bateman.json.lens._
import org.scalawag.bateman.json.literal._

val f = jsona"""
  [
    "a",
    4,
    false
  ]
""".asRootFocus
  
f(item(2))
```

To streamline your lenses even more, you can use a bare integer as an alias
for the `item` lens. Of course, this will only work in places where the
compiler is expecting a lens.

```scala mdoc:bateman:right:focus
f(1)
```

If you want to optionally descend to an index, use the `?` modifier.

```scala mdoc:bateman:right:some:focus
f(1.?)
```
```scala mdoc:bateman:right:none
f(5.?)
```
