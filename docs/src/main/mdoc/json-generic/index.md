@@@ index
* [X](config/index.md)
* [X](discriminators.md)
* [X](missing.md)
* [X](source.md)
@@@

# Generic Support

For many domains, the best data model is an ADT (or set of ADTs) representing
the domain. For this reason, bateman includes derivation of codecs for case 
classes and sealed traits. 

## Automatic Derivation

As long as there are appropriate codecs for all
the fields of a case class, you can derive a codec automatically.

```scala mdoc:bateman:jany
import org.scalawag.bateman.json._
import org.scalawag.bateman.json.syntax._

case class Circle(radius: Int, x: Int, y: Int, color: String)

locally {
  import org.scalawag.bateman.json.generic.auto._

  Circle(17, 0, 5, "red").toJAny
}
```

@@@ note
There are a few caveats to using automatic deriviation:

 - If you want to customize your codecs, you are limited to 
   @ref:[configuration](config/index.md) changes only.
 - Every place in your code that needs a codec will derive another (possibly 
   identical) one. This can dramatically increase compile times. A simple 
   solution is to "cache" codecs by assigning them to implicit terms that
   are in scope everywhere you need them. This is usually much more 
   straightforward using
   @ref[semiautomatic derivation](#semiautomatic-derivation). An easy way to 
   make sure that you don't accidentally get into this situation is to avoid 
   importing the automatic derivation package altogether.
@@@

## Semiautomatic Derivation

You can also derive a codec semiautomatically. This means that you have to 
explicitly request that the codec be created, but the details are handled 
for you. This gives you more control of the number of the codecs created and 
allows you to explicitly specify @ref:[configuration](config/index.md) and 
@ref:[discriminator](discriminators.md) parameters.

If you expect your consumers to generate their own codecs, you have no 
control over their configuration. If you explicitly create them as members 
of your companion classes, you have (almost) complete control.

```scala mdoc:bateman:jany
case class Rectangle(width: Int, height: Int, x: Int, y: Int, color: String)

object Rectangle {
  import org.scalawag.bateman.json.generic.semiauto.unchecked._
  implicit val encoder: JObjectEncoder[Rectangle] = deriveEncoderForCaseClass()
}

Rectangle(4, 8, 1, 2, "puce").toJAny
```

Be aware that, if you have a complex model where case classes contain 
references to other case classes, you will have to derive codecs for the 
inner classes before you'll be allowed to derive codecs for the outer 
classes. Without doing this, you'll get missing implicits for the inner 
codecs.  
