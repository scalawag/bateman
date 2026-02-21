## Upward navigation

@@@ note
All examples on this page use the following preface.
```scala mdoc:bateman:focus
import org.scalawag.bateman.json._
val deepFocus = JObject("a" -> JObject("b" -> JObject("c" -> JNull)))
  .asRootFocus
  .field("a")
  .flatMap(_.asObject)
  .flatMap(_.field("b"))
  .flatMap(_.asObject)
  .flatMap(_.field("c"))
  .getOrThrow
```
@@@

### Refocus on the parent

The `parent` JFocus extension method returns the parent of the focused 
value (its containing array or object).

```scala mdoc:bateman:focus
deepFocus.parent
```

### Refocus on the root

The `root` JFocus extension method returns its root value (its outermost 
containing array or object).

```scala mdoc:bateman:focus
deepFocus.root
```