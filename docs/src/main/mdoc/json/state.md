# State

@@@ note
The examples on this page use the following preface.
```scala mdoc:silent
import org.scalawag.bateman.json._
import org.scalawag.bateman.json.focus.JFocus
import org.scalawag.bateman.json.lens._
import org.scalawag.bateman.json.literal._
import org.scalawag.bateman.json.state.{root => stateRoot, _}

val json = json"""
  {
    "a": {
      "g": 4,
      "f": "thing",
      "b": true
    },
    "b": 6,
    "c": [11, 12, 13]
  }
""".asRootFocus
```
@@@

The state API provides a monadic way to navigate and edit JSON documents.
Rather than threading a @ref:[focus](focus/index.md) through a series of
`flatMap` calls yourself, you compose small state operations in
for-comprehensions and let the state monad thread the focus for you.

Each operation receives the current focus, potentially transforms the
document, and passes the resulting focus to the next operation. Errors
short-circuit through the chain via `JResult`.

## Types

There are two type aliases at the heart of the state API.

`State[A, B]` is a state monad over a `JFocus[A]` that produces a value of
type `B`. The focus type does not change across the operation.

`IndexedState[A, B]` is an indexed state monad where the focus value type
changes from `A` to `B`. Navigation, narrowing, and modification operations
use this type because they alter what the focus points at.

## Navigation

### Descending

Use `down` with a field name or array index to descend into the document.
It works with any @ref:[lens](lens/index.md), so you can descend through
multiple levels at once.

```scala mdoc:bateman:right:focus
val e1 = for {
  _ <- down("a")
  _ <- down("g")
} yield ()

e1.runS(json)
```

You can also pass a composite lens to `down` to navigate multiple levels
in a single step.

```scala mdoc:bateman:right:focus
val e2 = for {
  _ <- down("a" ~> "g")
} yield ()

e2.runS(json)
```

### Ascending

Use `up` to move the focus to the parent. It fails if the focus is already
at the root.

```scala mdoc:bateman:right:focus
val e3 = for {
  _ <- down("a")
  _ <- down("g")
  _ <- up
} yield ()

e3.runS(json)
```

### Root

Use `stateRoot` to jump the focus back to the document root from anywhere.
(Imported as `stateRoot` here to avoid ambiguity with the lens `root`.)

```scala mdoc:bateman:right:focus
val e4 = for {
  _ <- down("a")
  _ <- down("g")
  _ <- stateRoot
} yield ()

e4.runS(json)
```

## Extracting Values

### focus and value

Use `focus` to get the current focus as the output value or `value` to get
just the JSON value at the current focus. Neither changes the state.

```scala mdoc:bateman:right:jany
val e5 = for {
  _ <- down("b")
  v <- state.value[JAny]
} yield v

e5.runA(json).map(_.stripLocation)
```

### decode

Use `decode` to decode the value at the current focus into a Scala type.
The focus does not move.

```scala mdoc:bateman:right:value
val e6 = for {
  _ <- down("a")
  _ <- down("f")
  v <- decode[String]
} yield v

e6.runA(json)
```

### decodeThrough

Use `decodeThrough` to reach into the document via a lens and decode the
value found there -- without moving the focus. This is handy when you
need to pull a value from a sibling or child path without leaving your
current position.

```scala mdoc:bateman:right:value
val e7 = for {
  _ <- down("a")
  f <- decodeThrough[String]("f")
  g <- decodeThrough[Int]("g")
} yield (f, g)

e7.runA(json)
```

`decodeThrough` also works with cursor lenses to decode multiple values at
once.

```scala mdoc:bateman:right:list:value
val e8 = for {
  _ <- down("c")
  vs <- decodeThrough[BigDecimal](items ~> narrowTo[JNumber])
} yield vs

e8.runA(json)
```

## Narrowing

Use `narrow` to assert that the focused value is a specific JSON type.
This is the state equivalent of calling `narrow` on a focus directly.

```scala mdoc:bateman:right:focus
val e9 = for {
  _ <- down("a")
  _ <- narrow[JObject]
} yield ()

e9.runS(json)
```

If the value is not of the expected type, the operation fails.

```scala mdoc:bateman:left:errors
val e10 = for {
  _ <- down("b")
  _ <- narrow[JString]
} yield ()

e10.runS(json)
```

## Modification

### replace

Use `replace` to swap out the focused value. The replacement is encoded via
the implicit `Encoder`, so you can pass domain types directly.

```scala mdoc:bateman:jany
val e11 = for {
  _ <- down("a")
  _ <- down("b")
  _ <- replace(JNull)
  _ <- up
  _ <- down("f")
  _ <- replace(true)
} yield ()

e11.runS(json).getOrThrow.root.value
```

### modify

Use `modify` to transform the focused value using a pure function. The
result is encoded to JSON via the implicit `Encoder`.

```scala mdoc:bateman:jany
val e12 = for {
  _ <- down("a")
  _ <- down("g")
  _ <- modify((_: JAny) => "replaced")
} yield ()

e12.runS(json).getOrThrow.root.value
```

### modifyF

The fallible variant `modifyF` accepts a function that returns a `JResult`.

```scala mdoc:bateman:jany
import cats.syntax.either._
val e13 = for {
  _ <- down("a")
  _ <- down("g")
  _ <- narrow[JNumber]
  _ <- modifyF((n: JNumber) => (n.toBigDecimal * 2).rightNec[JError])
} yield ()

e13.runS(json).getOrThrow.root.value
```

### modifyFocus and modifyFocusF

`modifyFocus` and its fallible counterpart `modifyFocusF` are like `modify`
and `modifyF`, but the function receives the entire focus rather than just
the value. This gives access to the pointer, parent, and other contextual
information.

```scala mdoc:bateman:jany
val e14 = for {
  _ <- down("a")
  _ <- down("g")
  _ <- narrow[JNumber]
  _ <- modifyFocus((f: JFocus[JNumber]) => f.value.toBigDecimal * 2)
} yield ()

e14.runS(json).getOrThrow.root.value
```

## Deletion

Use `delete` to remove the focused value and move the focus to its parent.

```scala mdoc:bateman:jany
val e15 = for {
  _ <- down("a")
  _ <- down("b")
  _ <- delete()
} yield ()

e15.runS(json).getOrThrow.root.value
```

## Encoding

### encode

Use `encode` to encode a Scala value and replace the current focus with
the result.

```scala mdoc:bateman:jany
val e16 = for {
  _ <- down("a")
  _ <- down("g")
  _ <- encode(42)
} yield ()

e16.runS(json).getOrThrow.root.value
```

### encodeTo

Use `encodeTo` to encode a value and write it to a location specified by a
lens, creating intermediate structure as needed. The focus remains at its
current position in the updated document.

```scala mdoc:bateman:jany
val e17 = for {
  _ <- down("a")
  _ <- encodeTo("x" ~> "y", 89)
} yield ()

e17.runS(json).getOrThrow.root.value
```

`encodeTo` also accepts an `Option`. When `None`, the document is left
unchanged.

```scala mdoc:bateman:jany
val e18 = for {
  _ <- encodeTo("x", Some(89))
} yield ()

e18.runS(json).getOrThrow.root.value
```

## Composing a Multi-Step Edit

The real power of the state API is composing many small operations into a
single pipeline. Here's an example that navigates, modifies, and encodes
across several locations in one for-comprehension.

```scala mdoc:bateman:jany
val pipeline = for {
  _ <- down("a")
  _ <- down("g")
  _ <- narrow[JNumber]
  _ <- modifyFocus((f: JFocus[JNumber]) => f.value.toBigDecimal * 10)
  _ <- up
  _ <- down("f")
  _ <- replace("updated")
  _ <- up
  _ <- up
  _ <- encodeTo("d", "new field")
} yield ()

pipeline.runS(json).getOrThrow.root.value
```
