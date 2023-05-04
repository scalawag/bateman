@@@ index
* [X](json/index.md)
* [X](json-generic/index.md)
* [X](jsonapi/index.md)
* [X](literals.md)
* [X](enumeratum.md)
@@@

# bateman

bateman is a [cats](https://typelevel.org/cats/)-based
[Scala](https://www.scala-lang.org/) library for encoding and 
decoding [JSON](https://www.json.org/) and [JSON:API](https://jsonapi.org/) 
documents.

Yes, this _really_ is yet another JSON library for Scala. It started out as 
a JSON:API support library, but I realized that building it on top of 
existing JSON libraries left something to be desired, and it eventually 
ended up being a from-scratch JSON library as well.  

The primary goal is to maximize the developer experience when working with 
JSON:API. That includes both the service developer _and_ the developer 
calling the bateman APIs. Errors pinpoint the location of errors within 
parsed documents both at a structural level (using
[JSON Pointers](https://datatracker.ietf.org/doc/html/rfc6901)) and at a 
textual level (using line and column numbers). Due to this goal of producing 
precise, useful error messages, bateman does not depend on any other JSON 
libraries. 

## Major Concepts

  * A @ref:[`JAny`](json/model.md) is the root of an ADT that can represent 
    any kind of valid JSON value.
  * A @ref:[`JFocus`](json/focus/index.md) represents a deep reference to a 
    specific value _within_ a containing `JAny`. Usually, that containing 
    value will represent the root of the JSON text.
  * A @ref:[`JLens`](json/lens/index.md) is a transformation that can be
    applied to a focus to produce another one. This allows you to navigate 
    around within a `JAny` and interpret its contents. Lenses can be 
    composed to create complex traversals.

## Philosophy

The overall strategy in bateman to support leniency up to the point 
at which failure is inevitable. This is in contrast to the style where a 
validation pass is performed up front (e.g., when a JSON text enters the 
system) and then all code within the system assumes everything is in order. 
This up-front style doesn't really lend itself to multi-pass decoding (where 
different aspects of the text are reinterpreted and used for different 
purposes) and can make it hard to maintain the original context for precise 
error reporting.

The bateman (lenient) approach is to pass a representation of the original 
text around within your system and let each component extract what it needs. 
Failures only occur when a component tries to extract something and doesn't 
find what it expects.

that, after parsing a JSON text into its 
in-memory representation, you don't really do any up-front validation. You 
extract information from documents using the @ref:[ADT](json/model.md),
@ref:[lens operations](json/lens/index.md) or
state tranformations (TODO). It helps 
you to build decoders easily from case classes that represent what you're 
expecting to find in a given text. For JSON:API, this may mean that you grab 
the expected resources with a derived case class decoder but then make 
another query against the document to fetch the metadata that's not part of 
your model. Since this is all don't at query-time, it means that you can 
have multiple consumers regarding the JSON text in different ways and those 
consumers only see a failure if what they're trying to achieve can't be 
fulfilled.

An example:

> resource object v. resource identifier

## Getting Started

bateman is published to Maven Central and cross-built for Scala 2.12 and
2.13, so you can just add the following to your `sbt` build:

```scala
def bateman(artifact: String) = "org.scalawag.bateman" %% s"bateman-$artifact" % "0.1.3"

libraryDependencies ++= Seq(
  bateman("json"),            // core JSON functionality
  bateman("json-generic"),    // generic JSON codec derivation
  bateman("jsonapi"),         // core JSON:API functionality and document model
  bateman("jsonapi-generic"), // generic JSON:API codec derivation
  bateman("circe"),           // conversion to/from circe documents
  bateman("enumeratum"),      // custom JSON:API codec derivation
  bateman("literal")          // support for JSON string literals
)
```

## Use Cases

Things that you can do with bateman:

 * @ref:[parse a JSON text](json/parsing.md) into its 
   @ref:[in-memory representation](json/model.md)
 * @ref:[serialize a JSON value](json/serializing.md) to a JSON text 
 * @ref:[focus on a particular value](json/focus/index.md) within the JAny
 * @ref:[use lenses](json/lens/index.md) to create complex traversals
 * @ref:[decode the focused value](json/focus/operations/any/decode.md) to a 
   domain-specific type
 * @ref:[write a custom decoder](json/decoder.md) to decode JSON to your 
   own types
 * encode a supported value to JSON
    * encode an unsupported value to JSON using a custom encoder
    * automatically derive a JSON object encoder from a case class
 * @ref:[automatically derive](json-generic/index.md) a codec from a case class
 * programmatically create a JAny in your source code
 * transform a JAny by @ref:[deleting](json/focus/operations/deep/delete.md)
   or @ref:[modifying](json/focus/operations/any/modify.md) the focus
 * add custom semantic validation to a (custom) decoder
 * validate JSON literals at compile time, including templates
 * JSON:API
   * extract values from the focus as part of a JSON:API-compliant document
   * automatically derive a JSON:API resource decoder from a case class
   * programmatically create a JAny representing a JSON:API document 
   * automatically derive a JSON:API resource encoder from a case class
   * provide API clients the ability to specify field sets and include paths 