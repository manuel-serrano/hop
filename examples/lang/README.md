This example shows how to define a new language and how to require modules
implemented in that language. This verfy simple example defines CSV
(comma separated value) a new language for literals.

For maximmal efficiency, the CSV loader uses the native Hop parser
implemented in Scheme. This example also illustrates how to bind
this CSV parser into JavaScript.
