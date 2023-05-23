# Decoding

Decoding is the process of taking JSON values and turning them into 
domain-specific (non-JSON) values. There are a number of built-in decoders 
for simple and common types in the 
@scaladoc:[Decoder](org.scalawag.bateman.json.Decoder$) companion class. 

## Custom Decoders

If the built-in decoders don't give you what you need, you can always create 
your own instance of the @scaladoc:[Decoder](org.scalawag.bateman.json.Decoder)
type class. For many types, you may be able to base your decoder on a 
built-in to save yourself some effort. `Decoder` has a cats
@scaladoc:[Functor](cats.Functor) instance, so you can create a decoder for 
a simple `String` wrapper class, by using the `map` extension method on
`Functor`.

```scala mdoc:bateman:right
import cats.syntax.functor._
import org.scalawag.bateman.json._

final case class MyString(value: String)

implicit val decoderForMyString: JStringDecoder[MyString] =
  JStringDecoder[String].map(MyString)
  
JString("foo").asRootFocus.decode[MyString]
```

If you need something more complex, you can just write the decoding logic 
manually. As long as you stick with the other focus methods and errors, your 
error messages will fit right in. 

```scala mdoc:bateman:right
import cats.syntax.either._
import cats.syntax.functor._
import org.scalawag.bateman.json._
import org.scalawag.bateman.json.focus.weak._

sealed trait MySum
final case class MyBoolean(value: Boolean) extends MySum
final case class MyNumber(value: Int) extends MySum

implicit val decoderForMySum: JAnyDecoder[MySum] =
  f => f.value match {
    case b: JBoolean => MyBoolean(b.value).asRight
    case s: JNumber => f.decode[Int].map(MyNumber)
    case _ => JsonTypeMismatch(f, JBoolean, JNumber).leftNec
  }

(JNumber(4): JAny).asRootFocus.decode[MySum]
```
```scala mdoc:bateman:right
(JBoolean(true): JAny).asRootFocus.decode[MySum]
```
```scala mdoc:bateman:left:errors
(JNumber(4.6): JAny).asRootFocus.decode[MySum]
```
```scala mdoc:bateman:left:errors
(JString("foo"): JAny).asRootFocus.decode[MySum]
```

If you have a custom product type, don't forget to use cats'
@scaladoc:[Parallel](cats.Parallel) to combine results if you want all of 
your errors aggregated.

```scala mdoc:bateman:right
import cats.syntax.either._
import cats.syntax.functor._
import cats.syntax.parallel._
import org.scalawag.bateman.json._
import org.scalawag.bateman.json.focus.weak._

final case class MyProduct(a: Int, b: String)

implicit val decoderForMyProduct: JAnyDecoder[MyProduct] =
  _.asObject.flatMap { f =>
    (
      f.field("a").decode[Int],
      f.field("b").decode[String]
    ).parMapN(MyProduct)
  }

(JObject("a" -> JNumber(4), "b" -> JString("foo")): JAny)
  .asRootFocus
  .decode[MyProduct]
```
```scala mdoc:bateman:left:errors
(JObject("a" -> JNumber(4.1), "b" -> JBoolean(true)): JAny)
  .asRootFocus
  .decode[MyProduct]
```

If you need to decode to a domain-specific case class, you should look into 
using bateman's @ref:[generic derivation](../json-generic/index.md). 

Similarly, if you need a decoder for an
[enumeratum](https://github.com/lloydmeta/enumeratum)-based enumeration 
bateman has a @ref:[module](../enumeratum.md) for that.  