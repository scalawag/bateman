## Replace the focused value

@@@ note
While the descriptions of these operations colloquially discuss document
modification as if the data structures were mutable, they all _actually_
create a copy of the root JSON value and return a focus to the corresponding
value in the new document.
@@@

All examples use the following input document.

```scala mdoc:bateman:focus
import org.scalawag.bateman.json._
import org.scalawag.bateman.json.lens._
import org.scalawag.bateman.json.literal._
val in = json"""
  {
    "a": 4,
    "b": [
      true,
      {
        "aa": 1,
        "bb": 12.4,
        "cc": null
      }
    ]
  }
""".asRootFocus("b" ~> 1 ~> narrowTo[JObject]).getOrThrow
```

### Replace with a JSON value

The `replace` extension method replaces the focused value with the
given value. The value is encoded to JSON via the implicit `Encoder`.

```scala mdoc:bateman:focus
val out1 = in.replace(JString("replaced"))
```

If you look at the root of the returned focus, you can see the modified
document with your changes.

```scala mdoc:bateman:jany
out1.root.value
```

### Replace with a domain value

Because `replace` uses an implicit `Encoder`, you can pass any value
for which an encoder exists. This example replaces the focused object with
an integer.

```scala mdoc:bateman:focus
val out2 = in.replace(42)
```

```scala mdoc:bateman:jany
out2.root.value
```
