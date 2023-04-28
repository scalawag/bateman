# Encoding

Encoding is the process of taking domain-specific values and turning them into 
JSON. There are a number of built-in encoders for simple and common types in 
the @scaladoc:[Encoder](org.scalawag.bateman.json.Encoder$) companion class. 

@@@ note
If you import `org.scalawag.bateman.json.syntax._`, you can use the
`toJAny` extension method on any type that has an implicit `Encoder` in 
scope to turn the value into JSON.
@@@

## Custom Encoders

If the built-in encoders don't give you what you need, you can always create 
your own instance of the @scaladoc:[Encoder](org.scalawag.bateman.json.Encoder)
type class. For many types, you may be able to base your encoder on a 
built-in to save yourself some effort. `Decoder` has cats
@scaladoc:[Functor](cats.Functor) and
@scaladoc:[Contravariant](cats.Contravariant) 
instances, so you can create an encoder for a simple `String` wrapper class, 
for example, by using the `contramap` extension method.

```scala mdoc:bateman:jany
import cats.syntax.contravariant._
import org.scalawag.bateman.json._
import org.scalawag.bateman.json.syntax._

final case class MyString(value: String)

implicit val encoderForMyString: JStringEncoder[MyString] =
  JStringEncoder[String].contramap(_.value)
  
MyString("foo").toJAny
```

If you need something more complex, you can just write the encoding logic 
manually. We have to cast the values to `MySum` because that's the only type 
for which we've created an `Encoder`. 

```scala mdoc:bateman:jany
import cats.syntax.either._
import cats.syntax.functor._
import org.scalawag.bateman.json._
import org.scalawag.bateman.json.focus.weak._

sealed trait MySum
final case class MyBoolean(value: Boolean) extends MySum
final case class MyNumber(value: Int) extends MySum

implicit val encoderForMySum: JAnyEncoder[MySum] = {
  case MyBoolean(b) => b.toJAny
  case MyNumber(n)  => n.toJAny
}

(MyBoolean(true): MySum).toJAny
```
```scala mdoc:bateman:jany
(MyNumber(46): MySum).toJAny
```

If you need to encode a domain-specific case class, you should look into 
using bateman's @ref:[generic derivation](../json-generic/index.md). 

Similarly, if you need an encoder for an
[enumeratum](https://github.com/lloydmeta/enumeratum)-based enumeration 
bateman has a @ref:[module](../enumeratum.md) for that.  