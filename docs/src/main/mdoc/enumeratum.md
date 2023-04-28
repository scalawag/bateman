# Enumerations

Enumerations are supported through the
[enumeratum](https://github.com/lloydmeta/enumeratum) library. You can declare
an enumeration
[as documented by enumeratum](https://github.com/lloydmeta/enumeratum#usage)
and mix in the `BatemanEnum` trait to get a codec automatically.

@@@ note
Make sure you depend on the `bateman-json-enumeratum` artifact this to unlock
this functionality.
@@@

```scala mdoc:bateman:right
import enumeratum._
import org.scalawag.bateman.json._
import org.scalawag.bateman.json.enumeratum._

sealed trait Color extends EnumEntry

object Color extends Enum[Color] with BatemanEnum[Color] {
  case object Red extends Color
  case object Green extends Color
  case object Blue extends Color
  
  val values = findValues
}

JString("Red").asRootFocus.decode[Color]
```
```scala mdoc:bateman:left:errors
JString("Purple").asRootFocus.decode[Color]
```
