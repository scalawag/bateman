# Use Defaults For Missing Fields

By default, derived decoders will allow the incoming JSON to omit any fields 
that have default values in the case class.

```scala mdoc:bateman:right
import org.scalawag.bateman.json._
import org.scalawag.bateman.json.literal._
import org.scalawag.bateman.json.generic.auto._

case class Eggs(grade: Char, quantity: Int = 12)

val noQuantity = json"""
  {
    "grade": "A"
  }
""".asRootFocus

noQuantity.decode[Eggs]
```

If you want to require those fields to be present, even in the presence of 
case class default values, you can use the configuration to achieve that.

```scala mdoc:bateman:left
import org.scalawag.bateman.json.generic._

locally {
  implicit val config: Config = Config(useDefaultsForMissingFields = false) 

  noQuantity.decode[Eggs]
}
```

@@@ note
If you're trying to make your field "optional," not in the sense that it 
can be omitted in the incoming JSON, but in the sense that it can be 
_specified_ as having no value, this is called
"@ref:[nullable](../../nullable.md)" in bateman.

Nullable values can have defaults (and can therefore be omitted from the 
input JSON) just like any other value.
@@@