# Class Name Mapping

By default, derived codecs use discriminator values based on the Scala 
names of the case classes they represent.

```scala mdoc:bateman:jany
import org.scalawag.bateman.json._
import org.scalawag.bateman.json.syntax._
import org.scalawag.bateman.json.generic.auto._

sealed trait Mammal
case class RingTailedLemur(name: String) extends Mammal
case class TwoToedSloth(name: String) extends Mammal

// This must be statically typed as the trait to use the trait encoder and
// encode a discriminator.
val mammal: Mammal = RingTailedLemur("Bob")
mammal.toJAny
```

You may not want this to be the case, given the different conventions for 
different data formats. For example, while Scala tends to use pascal-case 
for its identifiers, JSON often tends to use snake-case.

You can change the conversions programmatically, using one of the built-in 
case conversions.

```scala mdoc:bateman:jany
import org.scalawag.bateman.json.generic._
import org.scalawag.bateman.json.generic.naming._

locally {
  implicit val config: Config = Config(classNameMapping = PascalCase to SnakeCase)
  mammal.toJAny
}
```

If a simple case conversion doesn't work for you, you can also just specify
your own mapping function.

```scala mdoc:bateman:jany
locally {
  implicit val config: Config = Config(classNameMapping = _.reverse)
  mammal.toJAny
}
```

While the examples here are for encoding, the same setting configures derived 
decoders to look for discriminator values with their converted names as 
well.

If you need a more complex mapping from concrete classes to discriminator 
values (e.g., if your concrete classes all have the same simple name), you 
should look into
@ref:[specifying the discriminator mapping](../discriminators.md) 
at the time you derive the codec.