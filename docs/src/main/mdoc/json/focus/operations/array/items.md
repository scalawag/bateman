### Refocus on all items

The
@scaladoc:[items](org.scalawag.bateman.json.focus.JFocusJArrayOps#items)
extension method returns a list of foci, one on each of the items in the 
focused JSON array. It returns a `List[JFocus[JAny]]`.

```scala mdoc:bateman:list:focus
import org.scalawag.bateman.json._
JArray(JNumber(4), JBoolean(false), JString("XXX"))
  .asRootFocus
  .items
```
