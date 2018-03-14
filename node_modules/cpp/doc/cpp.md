${ var doc = require( "hopdoc" ) }
${ var path = require( "path" ) }
${ var ROOT = path.dirname( module.filename ) }

CPP
===

This is the documentation of the CPP module that implements the `cpp`
language. This is a wrapper of the C pre-processor facility. It may be used
to define and use CPP macros inside JavaScript code. This module should
not be directly loaded but used to specify a language when requiring another
module. For instance:

Use `require( "./example.js", "cpp" )`


Configuration
-------------

The CPP compiler reads the file `RCDIR/cpprc.json` if it exists into a
variable named `config` in the rest of this documentation. This file
may override default CPP configuration. The default configuration is:

```json
${ doc.include( ROOT + "/../lib/cpprc.json" ) }
```

The `commandLine` property value is used to build the actual C compiler
invokation. It is build with the expression:

```hopscript
util.format( rc.commandLine,
             config && config.cc || require( hop.config ).bigloo[ "c-compiler" ],
	     #:bigloo-debug() > 0 ? "" : config.disableLinenumOpt,
	     file, target );
```

Example
-------

The following example shows a source code that uses macro. It must
be loaded with:

```hopscript
require( "./example.js", "cpp" );
```

The example is defined as:

${ <span class="label label-info">example.js</span> }

```hopscript
${ doc.include( ROOT + "/example.js" ) }
```

The included file is defined as:

${ <span class="label label-info">op.js</span> }

```hopscript
${ doc.include( ROOT + "/op.js" ) }
```


