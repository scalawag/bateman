@@@ index
* [X](fields.md)
* [X](fieldsNamed.md)
* [X](fieldOption.md)
* [X](field.md)
@@@

# Foci with an object value

The operations in this section can be executed on any focus that has a 
`JObject` as its value. With a strong focus, these operations will only 
compile against a focus that has such a value.

With a weak focus, you can _attempt_ any of these operations, 
but if the focus does not have a `JObject` value, a `JsonTypeMismatch` 
error will be returned.  

@@toc { depth=2 }
