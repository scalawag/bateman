@@@ index
* [X](json/index.md)
* [X](json-generic/index.md)
* [X](jsonapi/index.md)
* [X](jsonapi-generic/index.md)
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
at which failure is inevitable. This is in contrast to a style where a 
validation pass is performed up front (e.g., when a JSON text enters the 
system) and then all code within the system assumes everything is in order. 
This up-front style doesn't really lend itself to multi-pass decoding (where 
different aspects of the text are reinterpreted and used for different 
purposes) and can make it hard to maintain the original context for precise 
error reporting.

The bateman approach is to pass a representation of the original text around,
without any up-front semantic validation, within your system and let each 
component extract what it needs when it needs it. Failures only occur when a 
component tries to _extract_ data and doesn't find what it expects.

You can extract information from documents using 
@ref:[focus methods](json/focus/index.md),
@ref:[lens operations](json/lens/index.md) or
@ref:[state API](json/state.md). It can also automatically help 
you to build decoders from case classes that represent what you're 
expecting to find in a given text.

For JSON:API, this may mean that you grab 
the expected resources with a derived case class decoder but then make 
another query against the document to fetch the metadata that's not part of 
your model using lenses. Since this is all done just-in-time, it means that 
you can have multiple consumers interpreting the JSON text in different ways 
and those consumers only trigger a failure if what they're trying to achieve 
can't be fulfilled.

## Getting Started

bateman is published to Maven Central and cross-built for Scala 2.12, 2.13 and
3 and also supports both the JVM and ScalaJS. You can just add the following 
to your `sbt` build:

```scala
def bateman(artifact: String) = "org.scalawag.bateman" %% s"bateman-$artifact" % revision

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
 * @ref:[focus on a particular value](json/focus/index.md) within a JAny
 * @ref:[use lenses](json/lens/index.md) to create complex traversals
 * @ref:[decode the focused value](json/focus/operations/any/decode.md) to a 
   domain-specific type
    * compose @ref:[basic lenses](json/lens/basic/index.md) like
      @ref:[field](json/lens/basic/field.md),
      @ref:[item](json/lens/basic/item.md), and
      @ref:[narrow](json/lens/basic/narrow.md)
    * use lenses with different @ref:[cardinalities](json/lens/cardinality.md)
      to match zero, one, or many values
 * @ref:[use the state API](json/state.md) to compose navigation and
   editing steps in for-comprehensions
 * @ref:[write a custom decoder](json/decoder.md) to decode JSON to your 
   own types
 * @ref:[encode](json/encoder.md) a supported value to JSON
     * write a @ref:[custom encoder](json/encoder.md) for your own types
     * @ref:[automatically derive](json-generic/index.md) an encoder from a
       case class
 * @ref:[automatically derive](json-generic/index.md) a codec from a case
   class, with @ref:[configurable](json-generic/config/index.md) field name
   mapping, default values, and unknown field handling
 * @ref:[use validated JSON literals](literals.md) at compile time, with
   interpolation
 * transform a JAny by @ref:[deleting](json/focus/operations/deep/delete.md),
   @ref:[replacing](json/focus/operations/any/replace.md), or
   @ref:[modifying](json/focus/operations/any/modify.md) the focused value
 * JSON:API
     * @ref:[decode](jsonapi/decoding.md) JSON:API documents into domain types
     * @ref:[encode](jsonapi/encoding.md) domain types into JSON:API documents
     * @ref:[automatically derive](json-generic/index.md) JSON:API resource
       codecs from case classes 