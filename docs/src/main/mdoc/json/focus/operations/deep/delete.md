## Delete the focused value

The `delete` JFocus extension method deletes the focused value and 
refocuses on its parent (with one fewer child, of course). This operation 
_actually_ creates a copy of the root JSON value and returns a deep focus to 
the copied value corresponding to the parent.

```scala mdoc:bateman:right:focus
import org.scalawag.bateman.json._
import org.scalawag.bateman.json.lens._
import org.scalawag.bateman.json.focus.weak._
import org.scalawag.bateman.json.literal._
val deepFocus = json"""
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
""".asRootFocus("b" ~> 1 ~> "bb").getOrThrow

val out = deepFocus.delete
```

If you look at the root of the returned focus, you can see the modified 
document with your changes.

```scala mdoc:bateman:right
out.map(_.root.value.spaces2)
```