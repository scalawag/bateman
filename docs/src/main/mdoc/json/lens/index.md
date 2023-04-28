@@@ index
* [X](basic/index.md)
* [X](cardinality.md)
@@@

# Lenses

Lenses give you a more expressive way to traverse a JSON structure than the
way that's provided by the @ref:[focus API](../focus/index.md). Lenses allow 
you to both read from and write to the focused values.

For example, take a look at the following document and query.

```scala mdoc:bateman:right:focus
import org.scalawag.bateman.json._
import org.scalawag.bateman.json.literal._
import org.scalawag.bateman.json.lens.{focus, _}
import org.scalawag.bateman.json.focus.weak._

val json = json"""
  {
    "a": {
      "d": [ 5, 8 ]
    },
    "b": {
      "d": []
    },
    "c": {
      "d": [9]
    }
  }
""".asRootFocus

json.asObject
  .flatMap(_.field("a"))
  .flatMap(_.asObject)
  .flatMap(_.field("d"))
  .flatMap(_.asArray)
  .flatMap(_.item(1))
  .flatMap(_.narrow[JNumber])
```

It achieves the goal, but there are a lot of preconditions that have to be 
validated before the answer can be returned.

  - The root element must be an JSON object.
  - It must have a field named `a`.
  - The value in that field must be a JSON object.
  - It must have a field named `b`.
  - The value in that field must be a JSON array.
  - That array must have at least two items.
  - The second item must be a JSON number.

Each of these preconditions has to be expressed in a separate focus API call 
and the nested `flatMap`s end up obscuring the gist of the operation.

Now take a look at the following composite lens (utilizing the
@ref:[field](basic/field.md) and @ref:[item](basic/item.md) lenses),
which achieves the same result with much less boilerplate.

```scala mdoc:bateman:right:focus
json(focus ~> "a" ~> "d" ~> 1)
```

It's much clearer what's being sought here, but the same preconditions are 
checked and errors are reported. By composing lenses in this way, you can make 
the gist of your navigation much more apparent.

In the above example, the @ref:[cardinality](cardinality.md) of the 
composite lens is `Id`, but that's not necessarily always the case. Suppose 
that you want to extract multiple values from sibling paths. You may end up 
with a lens that looks something like this.

```scala mdoc:bateman:right:list:focus
json(focus ~> ** ~> "d" ~> *).map(_.foci)
```

This is a `List` lens, which can return zero or more foci from an input focus.
To understand why the `map` call is tacked onto the end, read up about
@ref:[list lenses](cardinality.md#using-list-lenses).
