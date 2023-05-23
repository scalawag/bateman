# narrow

The `narrow` lens attempts to narrow the type of the focused JSON value to 
the specified `JAny` subtype (`JType`). It will fail if the focused value is 
not of the specified type.

This lens has `Creatable` @ref:[cardinality](../cardinality.md).

```scala mdoc:bateman:right:focus
import org.scalawag.bateman.json._
import org.scalawag.bateman.json.lens._
import org.scalawag.bateman.json.literal._

val f = json"""
  {
    "a": 4
  }
""".asRootFocus
  
f(narrow[JObject])
```

```scala mdoc:bateman:left:errors
f(narrow[JArray])
```
