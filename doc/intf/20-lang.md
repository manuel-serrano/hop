${ var doc = require( "hopdoc" ) }
${ var path = require( "path" ) }
${ var ROOT = path.dirname( module.filename ) }

Language definition
===================

A language is any JavaScript object that implements a compilation
function associated with the `Symbol.compiler` property. This function
accepts two parameters, a resolved name of file to be compiled, and an
optional `options` objects. If the `options` is undefined or if the
the `options.target` property is undefined, it is to the compiler to
decide where and how to communicate its result. A compilation result
must be an object with two properties:

  * `type`: the type of the result which with either be:
    * `filename`: the filename where the compiled file is stored;
    * `json`: a json string representing the result of the compilation;
    * `value`: an arbitrary JavaScript value converible into json;
    * `error`: the compilation has failed.
  * `value`: the actual value of the compilation.

If a compiler stores its result into a file its result will be similar
to:

```
{  type: "filename", value: "/tmp/foo.tmp/js" }
```


When the compiler is to decide the file name where to store its result,
it might make sense for it to use the standard Hop cache directory, whoses
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


