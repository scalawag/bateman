# Encode Default Values

By default, derived encoders will exclude fields that are set to their 
case class default value.

```scala mdoc:bateman:jany
import org.scalawag.bateman.json._
import org.scalawag.bateman.json.syntax._
import org.scalawag.bateman.json.literal._
import org.scalawag.bateman.json.generic.auto._

case class Eggs(grade: Char, quantity: Int = 12)

Eggs('A', 12).toJAny
```

Of course, with a non-default value, the field will be always be included.

```scala mdoc:bateman:jany
Eggs('A', 18).toJAny
```


If you want to include those fields even though they have their default 
value, you can use the configuration to achieve that.

```scala mdoc:bateman:jany
import org.scalawag.bateman.json.generic._

locally {
  implicit val config: Config = Config(encodeDefaultValues = true)

  Eggs('A').toJAny
}
```
