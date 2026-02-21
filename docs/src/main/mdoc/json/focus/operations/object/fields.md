### Refocus on all field values

The
@scaladoc:[fields](org.scalawag.bateman.json.focus.JFocusJObjectOps#fields)
extension method returns a list of foci, one on each of values of the 
fields of the focused JSON object. It returns a
`List[JFocus[JAny]]`.

```scala mdoc:bateman:list:focus
import org.scalawag.bateman.json._
JObject("a" -> JNumber(4), "b" -> JBoolean(false), "a" -> JString("XXX"))
  .asRootFocus
  .fields
```
