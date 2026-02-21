@@@ index
* [X](fieldNameMapping.md)
* [X](classNameMapping.md)
* [X](useDefaultsForMissingFields.md)
* [X](encodeDefaultValues.md)
* [X](allowUnknownFields.md)
@@@

# Configuration

Configuration allows you to change the way that derived codecs work, in some 
specific ways.

@@toc { depth=2 }

For automatic derivation, a custom configuration must be in implicit scope 
where the codec is implicitly generated.

For semiautomatic derivation, the default behavior is the same as that for 
automatic derivation -- it uses the value in implicit scope.

```scala mdoc:bateman:jany
import org.scalawag.bateman.json._
import org.scalawag.bateman.json.syntax._
import org.scalawag.bateman.json.generic._
import org.scalawag.bateman.json.generic.naming._
import org.scalawag.bateman.json.generic.semiauto.unchecked._

case class Computer(operatingSystem: String)

val computer = Computer("MacOS")
implicit val config: Config = Config(fieldNameMapping = CamelCase to SnakeCase)

locally {
  implicit val encoder: JObjectEncoder[Computer] = deriveEncoderForCaseClass()
  computer.toJAny
}
```

However, you can also override the configuration when you invoke the deriver 
by wholly replacing it with another configuration.

```scala mdoc:bateman:jany
locally {
  implicit val encoder: JObjectEncoder[Computer] =
    deriveEncoderForCaseClass(config = Config.default)
  computer.toJAny
}
```

You can also specify to transform the implicit configuration, if 
you just want to tweak one aspect of the configuration while leaving 
it mostly consistent with the rest of your system.

```scala mdoc:bateman:jany
locally {
  val fn = (_: Config).copy(fieldNameMapping = CamelCase to KebabCase)
  implicit val encoder: JObjectEncoder[Computer] = deriveEncoderForCaseClass(config = fn)
  computer.toJAny
}
```
