${ var doc = require( "@hop/hopdoc" ) }

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
 of the module. The supported builtin languages are:
   * `javascript`;
   * `html`;
   * `json`;
   * `hopscript`.
   
Modules are loaded differently depending on their source file suffix.

 * `.js`, the module is source file module. It is loaded as plain source
 code. The value returned by `require` is the `exports` module property.
 * `.json`, the module is a JSON file. The JSON object is parsed and
 returned as the result of the `require` call.
 * `.html`, the module is a HopScript HTML expression, which is the result
 of the `require` call.

When `id`is a directory, the loader looks in the directory for a file
named package.json to tell how to load the module.

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

#### Languages ####

The optional argument `language` is a string denoting an implementation
language for the module to be required. The builtin languages are:

  * `hopscript`
  * `hop`
  * `html`
  * `json`
  * `ecmascript5`
  * `ecmascript6`
  * `ecmascript2017`


Additional custom languages may also be defined. See chapter
[Language definition](20-lang.html). The optional `language` defaults
to `require.lang`.

### require.lang ###
[:@glyphicon glyphicon-tag parameter] 
The default language used to require other modules.

${ <span class="label label-warning">Note:</span> } Language
importation is recursive. That is, if a module `mod` is imported with
a language `lang`, all the modules imported by `mod` will be
considered implemented in the same language `lang`, unless a specific
language is specified on the `require` calls.

This can be changed by modified the value of the `require.lang`
attribute.

### DSL ###

Modules implemented in Hop DSL (Domain Specific Languages) that
extend the Hop syntax by implementing their parser with the builtin
machinery of the Hop parser can auto-declare their implementation
language. In that case, the `require` calls that load them do not need
to specify any implementation language. The declaration is an extra
_use string_ to be included in the head of the program. For instance:

```hopscript
"use hiphop"
"use strict"

exports.prg = hiphop module( in A, in B, in R, out O ) {
   do {
      fork {
	 await now( A );
      } par {
	 await now( B );
      }
      emit O();
   } every( now( R ) )
}
```

The syntax of the _use string_ declaration is:

```ebnf
<UseDeclaration> --> use <Identifier>
```

The `Identifier` should be a module name resolvable using 
`require.resolve( Identifier )`.


### Client Side modules ###

Modules can be imported from either server-side or client-side code.
However, a module can be imported from a client-side. For that, it
must be first mentionned in a `script` tag of the head of the web
page, using the special attribute `module`. Then, it can be required
using the same syntax as any regular server-side module. The `src`
attribute of the `script` tag must exactly match the path mentioned
in the `require` call. See [API HTML](01-html.html) for details.


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

${ <span class="label label-info">requirec/example.json</span> }

```hopscript
${ doc.include( doc.EXAMPLES_DIR + "/requirec/example.json", 0 ) }
```

${ <span class="label label-info">requirec/mod.html</span> }

```hopscript
${ doc.include( doc.EXAMPLES_DIR + "/requirec/mod.html", 0 ) }
```

Package.json
------------

Hop extends regular `package.json` files with two entries.

### server ###

This entry, when present, overrides the regular `main` entry. This enables
writing npm packages compatible for both Hop and Node, when the two
implementations are different. Example

```json
{ 
  "server: "lib/myapp.js",
  "main": "lib/nodejs/myapp.js"
}
```

### client ###

This entry gives the list of files that must be included when then module
is loaded from a browser. Example

```json
{ "client": [ "lib/utils.js", "lib/message.js", "lib/client.js" ]}
```


ES6 Modules
-----------

Hop supports ES6 modules [ECMAScript 6][es6-export], [ECMAScript 2018][es2018-import]. With an extension to the `import` form. The path of the file
to be imported can be:

```ebnf
<ModulePath> --> "a static string"
  | <HopBuiltinModule>

<HopBuiltinModule> --> hop.<ident>
```

Example

```hopscript
import * as sp from hop.spage;
```

[es6-export]:
  https://www.ecma-international.org/ecma-262/6.0/#sec-exports 
[es2018-import]:
  https://www.ecma-international.org/ecma-262/9.0/#sec-imports
