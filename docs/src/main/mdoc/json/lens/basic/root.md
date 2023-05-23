# root

The `root` lens navigates to the outermost JSON value. This is determined by 
the value on which `asRootFocus` was called.

This lens has `Id` @ref:[cardinality](../cardinality.md).

```scala mdoc:bateman:right:focus
import org.scalawag.bateman.json._
import org.scalawag.bateman.json.lens._
import org.scalawag.bateman.json.literal._

val f = json"""
  {
    "a": {
      "b": 4
    }
  }
""".asRootFocus
  
f("a" ~> root)
```
