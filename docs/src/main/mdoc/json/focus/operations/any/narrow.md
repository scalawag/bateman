## Refocus on a narrower type

### Directly

There's an extension method for each JSON type (`asNull`, `asArray`, 
`asObject`, `asString`, `asNumber` and `asBoolean`) that attempts to narrow 
the focused value to the indicated JSON type. 

```scala mdoc:bateman:right:focus
import org.scalawag.bateman.json._
JBoolean(true).asRootFocus.asBoolean
```

If the value is _not_ actually of the specified type, a `JsonTypeMismatch`
is returned.

```scala mdoc:bateman:left:errors
import org.scalawag.bateman.json._
JBoolean(true).asRootFocus.asString
```

### Indirectly

The `narrow` JFocus extension method attempts to narrow the focused value to 
a specific JSON type (a `JAny`) using a type parameter instead. 

```scala mdoc:bateman:right:focus
import org.scalawag.bateman.json._
JNumber(4).asRootFocus.narrow[JNumber]
```

If the value is _not_ actually of the specified type, a `JsonTypeMismatch` 
is returned.

```scala mdoc:bateman:left:errors
import org.scalawag.bateman.json._
JNumber(4).asRootFocus.narrow[JString]
```

