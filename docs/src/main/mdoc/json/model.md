# Model

The in-memory representation of JSON that bateman uses is a recursive 
[algebraic data type](https://en.wikipedia.org/wiki/Algebraic_data_type) (ADT).
It hopefully looks a lot like you would expect. The abstract root type
@scaladoc[JAny](org.scalawag.bateman.json.JAny) represents _any_ legal 
JSON value. It has a concrete subclass representing each JSON type.

|case class|represents|
|-|-|
|@scaladoc[JNull](org.scalawag.bateman.json.JNull)|JSON `null`|
|@scaladoc[JArray](org.scalawag.bateman.json.JArray)|JSON array|
|@scaladoc[JObject](org.scalawag.bateman.json.JObject)|JSON object|
|@scaladoc[JString](org.scalawag.bateman.json.JString)|JSON string|
|@scaladoc[JNumber](org.scalawag.bateman.json.JNumber)|JSON number|
|@scaladoc[JBoolean](org.scalawag.bateman.json.JBoolean)|JSON boolean (`true` or `false`)|

A `JAny` contains a source location metadata _if and only if_ it was parsed 
from a JSON text. Any `JAny` created programmatically should not contain a
location.

@@@warning
Note that there's nothing keeping you from programmatically adding a
location when you create a `JAny` except for sound judgement, but nothing can 
really come of it other than confusing yourself or your users. Don't do it!
@@@

A few idiosyncracies about the individual `JAny` subtypes:

 * `JObject` contains a list of fields instead of a map. There are a few 
   reasons for this:
    * This allows it to maintain the order of the fields during both encoding 
      and decoding.
    * The JSON specification allows for multiple fields to have the same key. 
      Even though this seems like an edge case, it's supported in bateman. 
      Certain operations will fail if there are multiple fields with the same 
      key, but others support duplicate keys without error.
 * `JNull` is not a singleton, because it may contain a source location.
 * `JBoolean` is similarly not an ADT of two singletons.
 * `JNumber` stores its value as a string to preserve both the format and 
   the precision of its value. Invalid numbers will be detected at the time 
   of creation (programmatically or through parsing), but semantic errors 
   that may arise from trying to treat a number as a particular numeric type 
   will only happen once that conversion is attempted (e.g., treating `4.7` 
   as an `Int).  
