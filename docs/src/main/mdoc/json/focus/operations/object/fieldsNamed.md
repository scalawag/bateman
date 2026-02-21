### Refocus on all field values with a given name

The
@scaladoc:[fields(String)](org.scalawag.bateman.json.focus.JFocusJObjectOps#fields(String))
extension method returns a list of foci, one on each of values of the
fields with the specified name on the focused JSON object. It returns a
`List[JFocus[JAny]]`.

```scala mdoc:bateman:list:focus
import org.scalawag.bateman.json._
JObject("a" -> JNumber(4), "b" -> JBoolean(false), "a" -> JString("XXX"))
  .asRootFocus
  .fields("a")
```

If no fields exist with the specified name, an empty list is returned.

```scala mdoc:bateman:nil
import org.scalawag.bateman.json._
JObject("a" -> JNumber(4), "b" -> JBoolean(false), "a" -> JString("XXX"))
  .asRootFocus
  .fields("c")
```
