### Refocus on the item at the specified index

The
@scaladoc:[item(String)](org.scalawag.bateman.json.focus.JFocusJArrayOps#item)
extension method attempts to create a focus on the value at the specified 
index in the focused JSON array. It returns a `JResult[JFocus[JAny]]`. As 
opposed to @ref:[itemOption](itemOption.md), the absence of the index is 
considered an error.

If there is an item at the specified index, a focus to its value is 
returned in a `Right[JFocus[JAny]]`.

```scala mdoc:bateman:right:focus
import org.scalawag.bateman.json._
JArray(JNumber(4), JBoolean(false), JString("XXX"))
  .asRootFocus
  .item(2)
```

If there is no item at the specified index, an error is returned.

```scala mdoc:bateman:left:errors
import org.scalawag.bateman.json._
JArray(JNumber(4), JBoolean(false), JString("XXX"))
  .asRootFocus
  .item(3)
```
