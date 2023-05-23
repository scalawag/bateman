@@@ index
* [X](items.md)
* [X](itemOption.md)
* [X](item.md)
@@@

# Foci with an array value

The operations in this section can be executed on any focus that has a 
`JArray` as its value. With a strong focus, these operations will only 
compile against a focus that has such a value.

With a weak focus, you can _attempt_ any of these operations, 
but if the focus does not have a `JArray` value, a `JsonTypeMismatch` 
error will be returned.  

@@toc { depth=2 }
