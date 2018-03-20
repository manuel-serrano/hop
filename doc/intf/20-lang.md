${ var doc = require( "hopdoc" ) }
${ var path = require( "path" ) }
${ var ROOT = path.dirname( module.filename ) }

Language definition
===================

A Hop.js language is a DSL or a complete language that can be used to
implement an entire module. That is, one module cannot mix several
languages, but a module implemented in a language L1 can import
modules implemented in other languages. It belongs to the client
module when it imports (or /requires/) a module to specify the
language this imported module is implemented into. See chapter
[HopScript modules](01-module.html). for details on module
importations.

Language implementation
=======================

A language is any JavaScript object that implements a compilation
function associated with the `Symbol.compiler` property. This function
accepts two parameters, a resolved name of file to be compiled, and an
optional `options` object. If the `options` is undefined or if the the
`options.target` property is undefined, it is to the compiler to
decide where and how to communicate its result to the Hop runtime
system. A compilation result must be an object with two properties:

  * `type`: the type of the result which can either be:
    * `filename`: denoting that the compilation result as been stored in
 a file;
    * `json`: denoting that the compilation result is a json;
    * `value`: denoting that the compilation result is an arbitrary
 JavaScript value convertible into json;
    * `error`: denoting a compilation failure.
  * `value`: the actual value of the compilation.

For instance, if the language compiler wants to communicate that its
result is store into a file, it should return a JavaScript object like:

```
{ type: "filename", value: "/tmp/foo.tmp/js" }
```


When the compiler is to decide the file name where to store its result,
it might make sense for it to use the standard Hop cache directory, whose
name might be obtained with:

```
require( hop.config ).cacheDir
```


#### Example ####

${ doc.include( doc.EXAMPLES_DIR + "/lang/README.md" ) }

Let's start with a sample csv file.

${ <span class="label label-info">lang/sample.csv</span> }
```
${ doc.include( doc.EXAMPLES_DIR + "/lang/sample.csv" ) }
```

The client file requires the `sample.csv` file as if it was a regular
Hop.js module, but it specifies that this module is implemented in the
`csvg.js` language.

${ <span class="label label-info">lang/lang.js</span> }
```hopscript
${ doc.include( doc.EXAMPLES_DIR + "/lang/lang.js", 14 ) }
```

The `csv.js` language is implemented as follows:

${ <span class="label label-info">lang/csv.js</span> }
```hopscript
${ doc.include( doc.EXAMPLES_DIR + "/lang/csv.js", 14 ) }
```

The JavaScript language is a mere wrapper of the native Hop csv parser
that is implemented as:

${ <span class="label label-info">lang/csv.hop</span> }
```hop
${ doc.include( doc.EXAMPLES_DIR + "/lang/csv.hop", 15 ) }
```


