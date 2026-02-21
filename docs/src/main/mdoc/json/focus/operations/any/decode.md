## Decode to a domain-specific type

### Implicitly

The `decode` extension method of a focus attempts to turn JSON value 
into a domain-specific (non-JSON) value using a
@ref:[decoder](../../../decoder.md).

```scala mdoc:bateman:right
import org.scalawag.bateman.json._
JNumber(8).asRootFocus.decode[Int]
```

If the value is _not_ actually of the specified type, a `JsonTypeMismatch`
is returned. Note that we have to cast the `JBoolean` to a `JAny` to get 
this snippet to even compile. If we don't, the compiler knows it will fail 
before it even tries to run it because there's no decoder (by default) that 
can turn a `Boolean` into an `Int`. 

```scala mdoc:bateman:left:errors
import org.scalawag.bateman.json._
(JBoolean(true): JAny).asRootFocus.decode[Int]
```

### Explicitly

Using the same extension method, you can specify the decoder explicitly 
instead of relying on an implicit search.

```scala mdoc:bateman:right
import org.scalawag.bateman.json._
val decoder = Decoder.jstringToJNumber.andThen(Decoder.jnumberToIntDecoder)
JString("808").asRootFocus.decode(decoder)
```
