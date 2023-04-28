# Custom Discriminators

Abstract codecs need discriminator fields to indicate which concrete codec 
is ultimately responsible for the encoding or decoding. By default, bateman 
uses the JSON field `type` with a value that corresponds to the simple name 
of the concrete codec's case class.

```scala mdoc:bateman:jany
import org.scalawag.bateman.json._
import org.scalawag.bateman.json.syntax._
import org.scalawag.bateman.json.generic.semiauto.unchecked._

sealed trait Pet

case class Cat(name: String, lives: Int = 9) extends Pet

object Cat {
  implicit val codec: JObjectCodec[Cat] = deriveCodecForCaseClass()
}

case class Dog(name: String, breed: String) extends Pet

object Dog {
  implicit val codec: JObjectCodec[Dog] = deriveCodecForCaseClass()
}

// This must be statically typed as the trait to use the trait encoder and
// encode a discriminator.
val pets: List[Pet] = List(Dog("Rover", "Chihuahua"), Cat("Kevin"))

locally {
  implicit val codec: JObjectCodec[Pet] = deriveCodecForTrait()

  pets.toJAny
}
```

There are many cases where you may not want this default behavior. Some 
examples include:

 - having a collision with a domain field named `type`
 - having multiple concrete classes with the same name
 - needing multiple levels of abstract codecs (and hence, discrimination)
 - needing discriminator values that have nothing to do with the Scala class 
   names, possibly because of backward compatibility

In all these cases, the answer is custom discriminators.

@@@ note
If all you want to do is change the case of the class names in discriminator 
values , a much easier solution to the problem is setting
@ref:[`classNameMapping`](config/classNameMapping.md) in your configuration.
@@@

## Discriminator Field

To change the discriminator field, specify it when you create the abstract 
(trait) codec.

```scala mdoc:bateman:jany
locally {
  implicit val encoder: JObjectEncoder[Pet] =
    deriveEncoderForTrait[Pet](discriminatorLens = "class")

  pets.toJAny
}
```

## Discriminator Values

To change the discriminator values, specify the class-to-value mapping when 
you create the abstract (trait) codec.

```scala mdoc:bateman:jany
import org.scalawag.bateman.json.generic.Discriminators._

locally {
  implicit val encoder: JObjectEncoder[Pet] =
    deriveEncoderForTrait[Pet](
      discriminatorLens = "family",
      discriminator = CustomDiscriminator(
        forType[Cat]("felidae"),
        forType[Dog]("canidae")
      )
    )

  pets.toJAny
}
```

If you're missing any required types here, you'll get a runtime exception. 
Unfortunately, bateman can't check that you have all the cases covered at 
compile time.

@@@ note
You _can not_ change the discriminator field or values for automatically 
derived trait codecs. You must explicitly derive the codecs to use this
feature.
@@@
