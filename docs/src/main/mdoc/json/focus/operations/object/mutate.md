### Mutate the focused object

@@@ note
While the descriptions of these operations colloquially discuss document
modification as if the data structures were mutable, they all _actually_
create a copy of the root JSON value and return a focus to the corresponding
value in the new document.
@@@

All examples use the following input document.

```scala mdoc:bateman:focus
import org.scalawag.bateman.json._
import org.scalawag.bateman.json.lens._
import org.scalawag.bateman.json.literal._
val in = json"""
  {
    "a": 1,
    "b": 2,
    "c": 3
  }
""".asRootFocus
```

#### Append a field

The `append` extension method adds a new field after all existing fields.

```scala mdoc:bateman:jany
in.append("d", 4).value
```

#### Prepend a field

The `prepend` extension method adds a new field before all existing fields.

```scala mdoc:bateman:jany
in.prepend("z", 0).value
```

#### Insert a field at an index

The `insert` extension method adds a new field at the specified index
(zero-based).

```scala mdoc:bateman:jany
in.insert(1, "x", 99).value
```

#### Update a field value by index

The `updated` extension method replaces the value of the field at the
specified index.

```scala mdoc:bateman:jany
in.updated(0, "replaced").value
```

#### Delete a field by index

The `delete` extension method removes the field at the specified index.

```scala mdoc:bateman:jany
in.delete(0).value
```

#### Concatenate objects

The `++` extension method appends all fields from another object.

```scala mdoc:bateman:jany
(in ++ JObject("x" -> JNumber(8), "y" -> JNumber(9))).value
```

#### Write to a lens path

The `writeTo` extension method navigates a lens path from the focused
object, creating intermediate objects as needed, and writes the encoded
value at the target location.

```scala mdoc:bateman:right:jany
in.writeTo("d" ~> "e", "deep").map(_.value)
```

If the path passes through a non-object value, an error is returned.

```scala mdoc:bateman:left:errors
in.writeTo("a" ~> "nested", "fail")
```

#### Overwrite to a lens path

The `overwriteTo` extension method is similar to `writeTo`, but never
fails when a field is missing along the path. It creates intermediate
empty objects as needed. Compare this to `writeTo`, which would fail
because field `"d"` does not exist.

```scala mdoc:bateman:jany
in.overwriteTo("d" ~> "e", "deep").value
```

It also overwrites an existing field value. Here, `"a"` already has the
value `1`, but `overwriteTo` replaces it with the new value.

```scala mdoc:bateman:jany
in.overwriteTo("a", "replaced").value
```

It even replaces non-object intermediates. Here, `"a"` has the value `1`,
but `overwriteTo` replaces it with an object so it can continue navigating
to create the nested field.

```scala mdoc:bateman:jany
in.overwriteTo("a" ~> "nested", "forced").value
```
