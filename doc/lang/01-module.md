${ var doc = require( "hopdoc" ) }
${ var fontifier = require( "fontifier" ) }

HopScript Modules
=================

Hop.js supports [Nodejs Modules](https://nodejs.org/api/modules.html).
The import/exports mechanism, the file name resolution, the caching,
the `module` object, and the variable scoping are compatible in Hop.js and
Node.js. Hop.js adds several extensions to Nodejs Modules.


### require( id [, language ] ) ###
[:@glyphicon glyphicon-tag function]

The arguments are as follows:

 * `id` is a string that designates the source file containing the module.
 * `language` is an optional string denoting the implementation language
 of the module. The supported languages are:
   * `javascript`;
   * `hopscript`.
   
When a language is specified and when this language is not `hopscript`,
all the syntactic extensions of Hop.js are disabled (`service`, HTML syntax,
`${<span>$</span>}{`, and `${<span>~</span>}{` mark). Requiring a module
specifying the `javascript` language is then useful to require a module
that uses the extra HopScript keywords as normal identifiers.

Modules are loaded differently depending on their source file suffix.

 * `.js`, the module is source file module. It is loaded as plain source
 code. The value returned by `require` is the `exports` module property.
 * `.json`, the module is a JSON file. The JSON object is parsed and
 returned as the result of the `require` call.
 * `.html`, the module is a HopScript HTML expression, which is the result
 of the `require` call.

When `id`is a directory, the loader looks in the directory for a file named package.json
to tell how to load the module.

When `id` is an http url, Hop.js assumes that the file is to be
retrieved from a remote Hop.js server, and issues http requests to the
given server to get the file contents. Modules required within the
retrieved file are downloaded from the same location, except for
system modules which are assumed to be available locally.

#### Example ####

${ doc.include( doc.EXAMPLES_DIR + "/htmlr/README.md" ) }

${ <span class="label label-info">htmlr/htmlr.js</span> }

```hopscript
${ doc.include( doc.EXAMPLES_DIR + "/htmlr/htmlr.js", 14 ) }
```

${ <span class="label label-info">htmlr/htmlr.html</span> }

```hopscript
${ doc.include( doc.EXAMPLES_DIR + "/htmlr/htmlr.html", 14 ) }
```


### Client Side modules ###

Modules can be imported from either server-side or client-side code.
However, a module can be imported from a client-side only if it
has been cited in the `require` attribute of the `<head>` element
of the HTML document that is loaded on the client. The `require` attribute
can either be a string or an array of strings. See [API HTML](01-html.html)
for details.

#### Example ####

${ doc.include( doc.EXAMPLES_DIR + "/requirec/README.md" ) }

${ <span class="label label-info">requirec/requirec.js</span> }

```hopscript
${ doc.include( doc.EXAMPLES_DIR + "/requirec/requirec.js", 14 ) }
```

${ <span class="label label-info">requirec/mod1.js</span> }

```hopscript
${ doc.include( doc.EXAMPLES_DIR + "/requirec/mod1.js", 14 ) }
```

${ <span class="label label-info">requirec/mod2.js</span> }

```hopscript
${ doc.include( doc.EXAMPLES_DIR + "/requirec/mod2.js", 14 ) }
```

