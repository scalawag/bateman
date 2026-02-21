### Optionally refocus on the item at the specified index

The
@scaladoc:[itemOption(String)](org.scalawag.bateman.json.focus.JFocusJArrayOps#itemOption)
extension method attempts to create a focus on the value at the specified 
index in the focused JSON array. It returns a `JResult[Option[JFocus[JAny]]]`.

If there is an item at the specified index, a focus to its value is 
returned in a `Some[JFocus[JAny]]`.

```scala mdoc:bateman:some:focus
import org.scalawag.bateman.json._
JArray(JNumber(4), JBoolean(false), JString("XXX"))
  .asRootFocus
  .itemOption(2)
```

If there is no item at the specified index, `None` is returned.

```scala mdoc:bateman:none
import org.scalawag.bateman.json._
JArray(JNumber(4), JBoolean(false), JString("XXX"))
  .asRootFocus
  .itemOption(3)
```
