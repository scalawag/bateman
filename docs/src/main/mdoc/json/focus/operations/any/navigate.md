## Navigate to a JSON Pointer

The `navigate` method on a focus attempts to follow a `JPointer` from
the current position, descending into objects and arrays as directed by
the pointer's tokens.

@@@ note
All examples on this page use the following preface.
```scala mdoc:silent
import org.scalawag.bateman.json._
import org.scalawag.bateman.json.literal._
val json = json"""
  {
    "users": [
      {"name": "alice", "scores": [95, 87]},
      {"name": "bob", "scores": [70]}
    ]
  }
""".asRootFocus
```
@@@

### Navigate to a nested value

A `JPointer` consists of key (field name) and index (array index) tokens.
The `navigate` method follows each token in order.

```scala mdoc:bateman:right:focus
json.navigate(JPointer.Root.field("users").item(0).field("name"))
```

### Navigate into an array

Index tokens navigate into arrays by position.

```scala mdoc:bateman:right:focus
json.navigate(JPointer.Root.field("users").item(1))
```

### Handle navigation failures

If any token in the pointer doesn't match the structure of the document,
an error is returned. Here, `users` is an array, not an object, so a key
token fails.

```scala mdoc:bateman:left:errors
json.navigate(JPointer.Root.field("users").field("invalid"))
```

Similarly, an index token fails against a non-array value.

```scala mdoc:bateman:left:errors
json.navigate(JPointer.Root.field("users").item(0).field("name").item(0))
```
