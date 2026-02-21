### Refocus on a single field value with a specific name

The
@scaladoc:[field(String)](org.scalawag.bateman.json.focus.JFocusJObjectOps#field)
extension method attempts to create a focus on the value of a _single_ field 
with the specified name on the focused JSON object. It returns a
`JResult[JFocus[JAny]]`. As opposed to @ref:[fieldOption](fieldOption.md),
the absence of the field is considered an error.

If there is a single field with the specified name, a focus to its value is 
returned in a `Right[JFocus[JAny]]`.

```scala mdoc:bateman:right:focus
import org.scalawag.bateman.json._
JObject("a" -> JNumber(4), "b" -> JBoolean(false), "a" -> JString("XXX"))
  .asRootFocus
  .field("b")
```

If there are no fields with the specified name, an error is returned.

```scala mdoc:bateman:left:errors
import org.scalawag.bateman.json._
JObject("a" -> JNumber(4), "b" -> JBoolean(false), "a" -> JString("XXX"))
  .asRootFocus
  .field("c")
```

If there are multiple fields with the specified name, an error is returned.

```scala mdoc:bateman:left:errors
import org.scalawag.bateman.json._
JObject("a" -> JNumber(4), "b" -> JBoolean(false), "a" -> JString("XXX"))
  .asRootFocus
  .field("a")
```
