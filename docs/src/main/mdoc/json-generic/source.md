# Source Metadata

If you want to preserve some information about the JSON input from which 
your domain objects were sourced, you can ask bateman to store that 
information in your case class instances using the
@scaladoc:[@Source](org.scalawag.bateman.json.generic.Source) annotation.
This gives you the ability to pinpoint errors when doing semantic 
validation of your data post-decoding.

The metadata includes the "root" object, from which the instance was decoded.

```scala mdoc:bateman:value
import org.scalawag.bateman.json._
import org.scalawag.bateman.json.literal._
import org.scalawag.bateman.json.generic.Source
import org.scalawag.bateman.json.generic.decoding.JSource
import org.scalawag.bateman.json.generic.auto._

case class Guitar(
  brand: String,
  model: String,
  stringCount: Int = 6,
  @Source source: Option[JSource])

val guitar = json"""
  {
    "brand": "Gibson",
    "model": "Les Paul"
  }
""".asRootFocus.decode[Guitar].getOrThrow

guitar.source.get.root.pointer
```

It also contains foci to each of the values in the JSON from which the case 
class fields were derived. Note that this may be different than the name due 
to @ref:[field name mapping](config/fieldNameMapping.md).


Any fields that 
did not come from the JSON (`stringCount` in this example) will be absent in 
the source field map.

```scala mdoc:bateman:value
guitar.source.get.fields.map { case (n, v) =>
  s"$n -> ${v.pointer} (${v.value.location.get})"  
}.mkString("\n")
```
