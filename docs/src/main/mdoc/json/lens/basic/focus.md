# focus

The `focus` lens simply returns the same focus to which is it applied. 
This isn't very useful in and of itself, but it can be a useful way to 
tell the compiler that a lens is what follows by using it on the 
left-hand side of a composition arrow `~>`.

This lens has `Creatable` @ref:[cardinality](../cardinality.md).

```scala mdoc:bateman:right:focus
import org.scalawag.bateman.json.lens._
import org.scalawag.bateman.json.literal._

val f = json"""
  {
    "a": 4
  }
""".asRootFocus
  
f(focus)
```

Here's an example of using `focus` to create a lens without having to 
specify the type explictly. Depending on the context, this could make your 
code more readable.

```scala mdoc:bateman:value
focus ~> "a"
```
