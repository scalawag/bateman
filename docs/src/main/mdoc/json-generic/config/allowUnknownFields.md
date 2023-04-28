# Allow Unknown Fields

By default, derived decoders will ignore any fields in the incoming JSON 
that they don't require. This allows for more lax decoding.

```scala mdoc:bateman:right
import org.scalawag.bateman.json._
import org.scalawag.bateman.json.literal._
import org.scalawag.bateman.json.generic.auto._

sealed trait Vehicle
case class Car(make: String, model: String) extends Vehicle
case class Truck(axles: Int, tons: Int) extends Vehicle

val civic = json"""
  {
    "make": "honda",
    "model": "civic",
    "trim": "lx",
    "year": 1998
  }
""".asRootFocus

civic.decode[Car]
```

If you want the decoder to consider extraneous fields an error, you can use 
the configuration to achieve that. 

```scala mdoc:bateman:left
import org.scalawag.bateman.json.generic._

locally {
  implicit val config: Config = Config(allowUnknownFields = false)

  civic.decode[Car]
}
```

Any fields that are used as discriminators (by abstract decoders to get to the 
concrete decoder) will be excluded by this extraneous field check.

```scala mdoc:bateman:left
locally {
  implicit val config: Config = Config(allowUnknownFields = false)

  json"""
      {
        "type": "Truck",
        "make": "mack",
        "axles": 5,
        "tons": 20
      }
    """.asRootFocus.decode[Vehicle]
}
```

Note that `make` is considered extraneous, but the discriminator `type` is 
allowed. If you see a discriminator being rejected, odds are you're using 
the concrete decoder instead of the abstract one.

```scala mdoc:bateman:left
locally {
  implicit val config: Config = Config(allowUnknownFields = false)

  json"""
      {
        "type": "Truck",
        "axles": 5,
        "tons": 20
      }
    """.asRootFocus.decode[Truck] // concrete decoder specified!
}
```
