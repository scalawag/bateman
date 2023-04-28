# Cardinality

Each lens has a cardinality, which determines how many foci are produced 
when this lens is applied to a focus. It also determines the cardinality of 
any lenses created through composition with the lens.

 * `List` - returns zero or more foci for each input focus
 * `Option` - returns zero or one focus for each input focus
 * `Id` - returns exactly one focus for each input focus
 * `Creatable` - special case of Id that also supports insertion/writing

## Composition

When you compose two lenses, their cardinalities determine the cardinality 
of the resulting lens. Generally speaking, there's an ordering of precedence 
from `List` > `Option` > `Id` > `Creatable`. The resulting lens has the higher 
precedence of the two input lenses.

Here's a table, though, if you like a exhaustive explanation.

| left \ right | Creatable | Id     | Option | List |
|--------------|-----------|--------|--------|------|
| Creatable    | Creatable | Id     | Option | List |
| Id           | Id        | Id     | Option | List |
| Option       | Option    | Option | Option | List |
| List         | List      | List   | List   | List |

@@@ note
Composed lenses flatten everything. If you want to do a query that 
_doesn't_ flatten everything (e.g., that returns a list of lists of foci), 
don't use the lenses. Just use the calls directly on the focus.
@@@

## Creatable Lenses

Creatable lenses are a special case of Id lenses. A creatable lens returns 
exactly one focus for each input focus, just like a Id lens. However, it 
can also be used to insert a JSON value relative to the current focus. The
only creatable lens are the ones that refer to a specific field of an object.

## Making Option Lenses

You can make any `Id` or `Creatable` lens into an `Option` lens by adding 
the `?` modifier to it.

```scala mdoc:bateman:right:none
import org.scalawag.bateman.json.lens._
import org.scalawag.bateman.json.literal._

val f = json"""
  {
    "a": 17
  }
""".asRootFocus
  
val l = focus ~> "b" ~> 0 ~> "a"
f(l.?)
```

Note that the deep lens is what has been made optional. That allows it to 
fail anywhere along the path. That's different behavior that you would get 
if you only made the last lens optional. That would imply an error if 
anything were missing _prior_ to the optional lens in the traversal.

```scala mdoc:bateman:left:errors
f(focus ~> "b" ~> 0 ~> "a".?)
```

`List` lenses already allow for the empty result, so 
there's no need to make them optional.

# Using List Lenses

While `Id` lenses return an `Id[JFocus[JAny]]` and `Option` lenses return an
`Option[JFocus[JAny]]`, `List` do not return a `List[JFocus[JAny]]`. `List` 
lenses return something entirely different. They return a `JFoci` from 
which you can get a list of foci, but they exist because you can also do 
more with them. Since you can both read and write using a focus, this 
special return type allows you to modify multiple foci within the same 
document with one call.

Due to this additional wrapper, to get the list of foci from a list lens' 
return value, access the `foci` member on it.

```scala mdoc:bateman:right:list:focus
import org.scalawag.bateman.json._
import org.scalawag.bateman.json.literal._
import org.scalawag.bateman.json.lens.{focus, _}
import org.scalawag.bateman.json.focus.weak._

val json = json"""
  {
    "a": {
      "d": [ 5, 8 ],
      "e": null
    },
    "b": {
      "e": false
    },
    "c": {
      "d": [9],
      "e": 87
    }
  }
""".asRootFocus

val dLens = focus ~> ** ~> "d".?
json(dLens).map(_.foci)
```

You can also call one of the write methods on `JFoci`, such as `delete`, which 
will allow you to delete all the focused values at once, giving you a new
document reflecting all the changes.

```scala mdoc:bateman:right:some:jany
json(dLens).flatMap(_.delete).map(_.root.map(_.value))
```
