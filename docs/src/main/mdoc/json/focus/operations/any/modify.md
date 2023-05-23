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
import org.scalawag.bateman.json.focus.weak._
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
""".asRootFocus("b" ~> 1 ~> narrow[JObject]).getOrThrow
```

### Modify the focused value

The `modify` extension method modifies the focused value in-place. This
example replaces the focused object with its size.

```scala mdoc:bateman:focus
val out1 = in.modify(o => JNumber(o.fields.size))
```

If you look at the root of the returned focus, you can see the modified
document with your changes.

```scala mdoc:bateman:jany
out1.root.value
```

### Modify the focused value fallibly

The `modifyF` extension method also modifies the focused value in-place, but 
allows the modification function to fail. This example replaces the focused
object with the value of its `bb` key. 

```scala mdoc:bateman:right:focus
val out2 = in.modifyF(_.field("bb").value)
```

If you look at the root of the returned focus, you can see the modified
document with your changes.

```scala mdoc:bateman:right
out2.map(_.root.value.spaces2)
```

If the modification function fails, so does the `modifyF` call.

```scala mdoc:bateman:left:errors
in.modifyF(_.field("xx").value)
```

