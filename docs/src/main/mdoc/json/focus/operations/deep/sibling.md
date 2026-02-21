## Sibling navigation

@@@ note
All examples on this page use the following preface.
```scala mdoc:silent
import org.scalawag.bateman.json._
val deepFocus = JArray((0 to 5).map(JNumber(_)): _*).asRootFocus.item(3).getOrThrow
```
@@@

### Refocus on the first sibling

The `first` JFocus extension method returns the first sibling in the focused 
value's containing array or object.

```scala mdoc:bateman:focus
deepFocus.first
```

### Refocus on the last sibling

The `last` JFocus extension method returns the last sibling in the focused 
value's containing array or object.

```scala mdoc:bateman:focus
deepFocus.last
```

### Refocus on the previous sibling

The `previous` JFocus extension method returns the previous sibling in 
the focused value's containing array or object. 

```scala mdoc:bateman:right:focus
deepFocus.previous
```

If the focused item or field value is the first one in its parent, 
an error is returned.

```scala mdoc:bateman:left:errors
deepFocus.first.previous
```

### Refocus on the next sibling

The `next` JFocus extension method returns the next sibling in 
the focused value's containing array or object.

```scala mdoc:bateman:right:focus
deepFocus.next
```

If the focused item or field value is the last one in its parent, 
an error is returned.

```scala mdoc:bateman:left:errors
deepFocus.last.next
```
