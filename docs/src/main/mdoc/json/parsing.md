# Parsing

You can use bateman to parse a JSON text into its in-memory JSON 
representation (as a @ref:[JAny](model.md)) using the
@scaladoc:[parse](org.scalawag.bateman.json.index#parse(text:String,source:Option[String]):org.scalawag.bateman.json.parser.ParseResult[org.scalawag.bateman.json.focus.JRootFocus[org.scalawag.bateman.json.JAny]]) 
function.

```scala mdoc:bateman:right:focus
org.scalawag.bateman.json.parse("""
  {
    "a": 7,
    "b": false
  }
""")
```

If there is a syntax error, it will be indicated with the offending 
position.

```scala mdoc:bateman:left:syntax-error
org.scalawag.bateman.json.parse("""
  {
    "a": 7,
    "b": false,
  }
""")
```

If you want to specify a source name for your JSON text, you can use the 
second, optional parameter. This may be useful when you are parsing many 
JSON files, but it can be overkill when only parsing one. It also may be the 
case that you don't have a good source name (like a file name) if you are 
parsing from a stream. This name is only used in error reporting and its 
absence just causes it not to appear in the error messages, as shown above.

```scala mdoc:bateman:left:syntax-error
org.scalawag.bateman.json.parse("""
  {
    false
  }
""", Some("input.json"))
```

If you're parsing from a stream, there's no reason to buffer the text into a 
String to pass to the parse call. There's an override of 
@scaladoc:[parse](org.scalawag.bateman.json.index#parse(text:scala.collection.compat.immutable.LazyList[Char],source:Option[String]):org.scalawag.bateman.json.parser.ParseResult[org.scalawag.bateman.json.focus.JRootFocus[org.scalawag.bateman.json.JAny]]) 
that takes a 
`LazyList[Char]` for just this purpose. This is actually the primary parsing 
function. The `String` variants are just for convenience.
