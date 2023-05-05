# Encoding

[JSON:API](https://jsonapi.org/) documents are just JSON documents, so you 
can build them using any of the tools that you learned about in the 
@ref:[JSON](../json/index.md) section. 

bateman also includes a
@scaladoc:[JSON:API document model](org.scalawag.bateman.jsonapi.encoding.Document)
that you can use to help you build valid documents. It includes some 
guardrails for JSON:API, enforcing rules like "a document _must_ contain at 
least one of `meta`, `data` or `errors` but _must not_ contain both `data` 
and `errors`".

```scala mdoc:bateman:jany
import org.scalawag.bateman.json.syntax._
import org.scalawag.bateman.jsonapi.encoding._

val primary1 =
  ResourceObject("person", "0xBEEFED")
    .withAttribute("name", "Joshua")
    .withAttribute("birth_date", "1964-01-18")
    
val primary2 =
  ResourceObject("person", "0xABACAB")
    .withAttribute("name", "Phil")
    .withAttribute("birth_date", "1951-01-30")
    
val doc =
  Document.withData(List(primary1, primary2))
    .withJsonapi(Jsonapi(version = Some("1.0")))
    .withLink("self", "http://example.com/1234")

val jany = doc.toJAny
```

Using this builder will prevent you from making a lot of common mistakes in 
JSON:API documents.

You can also use the lenses provided by the `bateman-jsonapi` artifact to 
manipulate the JSON after you've already created a JSON:API document. You 
might want to do this to add a touch of metadata that's not present in your 
domain model, for example.

```scala mdoc:bateman:right:jany
import org.scalawag.bateman.jsonapi.lens._
import org.scalawag.bateman.json.focus.weak._

jany.asRootFocus(data ~> 1)
  .map(_.overwriteTo(meta("archived"), true))
  .map(_.root.value)
```
