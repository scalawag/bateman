JSON:API forbids the presence of both `errors` and `data` in a document.

You can create documents explicitly or you can use the builder. Explicit 
makes it possible to create bad documents (`data` + `errors`, `data which is 
a JSON string, etc.). The builder prevents many ofo these errors.

Document() creates an empty document with no data. At this point, you can 
add errors to turn it into an error document. To create a data document, you 
must specify the data at the time you create the document.
