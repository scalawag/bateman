## Modify the JSON document

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

### Modify the focused value

The `modify` extension method transforms the focused value using a
function that receives the value directly. This example replaces the
focused object with its size.

```scala mdoc:bateman:focus
val out1 = in.modify(o => JNumber(o.fieldList.size))
```

If you look at the root of the returned focus, you can see the modified
document with your changes.

```scala mdoc:bateman:jany
out1.root.value
```

### Modify the focused value fallibly

The fallible `modify` overload accepts a function that returns a
`JResult`, allowing it to fail. This example replaces the focused object
with its size, but only if it has fields.

```scala mdoc:bateman:right:focus
import cats.syntax.either._
val out2 = in.modify { o =>
  if (o.fieldList.nonEmpty) JNumber(o.fieldList.size).rightNec
  else JsonTypeMismatch(in, JNumber).leftNec
}
```

If the modification function fails, so does the `modify` call.

```scala mdoc:bateman:left:errors
in.modify(_ => JsonTypeMismatch(in, JNumber).leftNec)
```

### Modify using the focus

The `modifyFocus` extension method is similar to `modify`, but the
function receives the entire focus rather than just the value. This gives
the function access to the pointer, parent, and other contextual
information.

```scala mdoc:bateman:focus
val out3 = in.modifyFocus(f => JString(f.pointer.toString))
```

### Modify using the focus fallibly

The fallible `modifyFocus` overload accepts a function that returns a
`JResult`. This example navigates from the focus to extract a nested value.

```scala mdoc:bateman:right:focus
val out4 = in.modifyFocus(_.field("bb").map(_.value))
```

If the modification function fails, so does the `modifyFocus` call.

```scala mdoc:bateman:left:errors
in.modifyFocus(_.field("xx").map(_.value))
```
