### Optionally refocus on a single field value with a specific name

The
@scaladoc:[fieldOption(String)](org.scalawag.bateman.json.focus.JFocusJObjectOps#fieldOption)
extension method attempts to create a focus on the value of a _single_ field
with the specified name on the focused JSON object. It returns a
`JResult[Option[JFocus[JAny]]]`.

If there is a single field with the specified name, a focus to its value is 
returned in a `Right[Some[JFocus[JAny]]]`.

```scala mdoc:bateman:right:some:focus
import org.scalawag.bateman.json._
JObject("a" -> JNumber(4), "b" -> JBoolean(false), "a" -> JString("XXX"))
  .asRootFocus
  .fieldOption("b")
```

If there are no fields with the specified name, `Right[None]` is returned.

```scala mdoc:bateman:right:none
import org.scalawag.bateman.json._
JObject("a" -> JNumber(4), "b" -> JBoolean(false), "a" -> JString("XXX"))
  .asRootFocus
  .fieldOption("c")
```

If there are multiple fields with the specified name, an error is returned.

```scala mdoc:bateman:left:errors
import org.scalawag.bateman.json._
JObject("a" -> JNumber(4), "b" -> JBoolean(false), "a" -> JString("XXX"))
  .asRootFocus
  .fieldOption("a")
```
