### Mutate the focused array

@@@ note
While the descriptions of these operations colloquially discuss document
modification as if the data structures were mutable, they all _actually_
create a copy of the root JSON value and return a focus to the corresponding
value in the new document.
@@@

All examples use the following input document.

```scala mdoc:bateman:focus
import org.scalawag.bateman.json._
val in = JArray(JNumber(1), JNumber(2), JNumber(3)).asRootFocus
```

#### Append an item

The `append` extension method adds a new item after all existing items.

```scala mdoc:bateman:jany
in.append(4).value
```

#### Prepend an item

The `prepend` extension method adds a new item before all existing items.

```scala mdoc:bateman:jany
in.prepend(0).value
```

#### Insert an item at an index

The `insert` extension method adds a new item at the specified index
(zero-based).

```scala mdoc:bateman:jany
in.insert(1, 99).value
```

#### Update an item by index

The `updated` extension method replaces the item at the specified index.

```scala mdoc:bateman:jany
in.updated(0, "replaced").value
```

#### Delete an item by index

The `delete` extension method removes the item at the specified index.

```scala mdoc:bateman:jany
in.delete(0).value
```

#### Concatenate arrays

The `++` extension method appends all items from another array.

```scala mdoc:bateman:jany
(in ++ JArray(JNumber(8), JNumber(9))).value
```
