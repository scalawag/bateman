# Literals

bateman can check Scala JSON literals for syntax errors at compile time. 

First, you need to make sure that you're depending on the `json-literal` 
artifact and importing the literal package.

```scala mdoc:bateman:jany
import org.scalawag.bateman.json._
import org.scalawag.bateman.json.literal._

json"""
  {
    "a": 4,
    "b": true
  }
"""
```

The `json` string context expects that it will contain a JSON object. If the 
root value is anything else, you'll get an error during compilation.

There's another context `jsona` which is almost identical except that it 
expects a JSON array.

```scala mdoc:bateman:jany
jsona"[1,2,3,true,null]"
```

If you want to create something other than an object or array, just use the 
constructor for the appropriate type.

```scala mdoc:bateman:jany
JString("plantain")
```

## Interpolations

JSON literals may contain interpolations, as long as there is an implicit 
encoder in scope for the embedded types. Interpolations must replace JSON 
values in the text (i.e, no partial value or punctuation insertion is allowed).

```scala mdoc:bateman:jany
val inner = json"""{"a": 1, "b": 2}"""

json"""
  {
    "A": ${2 + 2},
    "B": $inner,
    "C": ${inner.fieldList.length}
  }
"""
```

## Location

If you use a JSON literal with _no_ interpolations, the resulting `JAny` 
will include location information as if it had been parsed. Note that these 
locations are rooted from the opening quote and not the containing Scala 
source file. 

```scala mdoc:bateman:some
jsona"[1,2,3,true,null]".items(3).location
```

If you include interpolations, all location information is stripped from the 
resulting `JAny` because variable insertion can arbitrarily change where 
everything is in the text, and bateman can't reliably determine the position
of the values.

```scala mdoc:bateman:none
jsona"[${1},2,3,true,null]".items(3).location
```
