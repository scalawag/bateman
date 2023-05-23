# Serializing

@@@ note
The examples on this page use the following preface.
```scala mdoc:silent
import org.scalawag.bateman.json._
val json = JObject(
  "d" -> JString("👍"),
  "c" -> JArray(JNumber(4), JNumber(8)),
  "b" -> JBoolean(false),
  "a" -> JNumber(7)
)
```
@@@


Any @ref:[JAny](model.md) can be serialized to a JSON text using its
@scaladoc:[render](org.scalawag.bateman.json.JAny#render(renderer:org.scalawag.bateman.json.Renderer):String) method, taking a
@scaladoc:[Renderer](org.scalawag.bateman.json.Renderer) that controls 
exactly how the resulting text is formatted. If you don't need anything 
extraordinary, you can probably just use one of the built-in renderers to get 
common renderings.

For a minimal (no extraneous whitespace) JSON text, just use the 
no-argument @scaladoc:[render](org.scalawag.bateman.json.JAny#render:String)
method.

```scala mdoc:bateman:value
json.render
```

For a prettier, more human-readable rending, use the
@scaladoc:[spaces2](org.scalawag.bateman.json.JAny#spaces2:String) method.

```scala mdoc:bateman:value
json.spaces2
```

## Customization

Some simple customization can be done by creating a new renderer out of the 
provided parts. @scaladoc:[AsciiOnly](org.scalawag.bateman.json.AsciiOnly) 
will defeat the use of non-ASCII characters in the JSON text. 

```scala mdoc:bateman:value
val renderer = new PrettyRenderer(3) with AsciiOnly
json.render(renderer)
```

@scaladoc:[SortedFields](org.scalawag.bateman.json.SortedFields)
will sort the fields of JSON objects by their keys in the resulting JSON text.

```scala mdoc:bateman:value
json.render(new PrettySpaces2 with SortedFields)
```

For anything more advanced, it's probably wise to look at one of the 
provided renderers and work from there. You probably don't want to change 
the way primitive values are rendered as much as you want to mess with the 
whitespace, so 
@scaladoc:[PrimitiveRenderers](org.scalawag.bateman.json.PrimitiveAppenders)
will generally prevent you from reinventing the wheel.
