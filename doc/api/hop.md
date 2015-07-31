${ var doc = require( "hopdoc" ) }

Hop
===

This module contains utilities for getting, controlling, and using the
Hop server.

Use `require( 'hop' )` to use this module.


Configuration
-------------

### hop.shareDir ###
The `share` directory of the Hop installation.

### hop.binDir ###
The `bin` directory of the Hop installation.

### hop.libDir ###
The `lib` directory of the Hop installation.

### hop.port ###
The port number of the running Hop server.

### hop.hostname ###
The host name of the running Hop server.

### hop.version ###
The Hop version.

```hopscript
console.log( "Hop version:", hop.version );
```


Responses
---------

### hop.HTTPResponseHop ###
[:@glyphicon glyphicon-tags]

### hop.HTTPResponseXml ###

### hop.HTTPResponseFile ###

#### Example ####

```hopscript
${ doc.include( doc.ROOT + "/examples/file/file.js", 13 ) }
```

### hop.HTTPResponseString ###

### hop.HTTPResponseAuthentication ###

#### Example ####

```hopscript
${ doc.include( doc.ROOT + "/examples/authentication/authentication.js", 13 ) }
```

### hop.HTTPResponseAsync ###

#### Example ####

${ doc.include( doc.ROOT + "/examples/async/doc.md" ) }

```hopscript
${ doc.include( doc.ROOT + "/examples/async/async.js", 13 ) }
```

### hop.HTTPResponseProxy ###

Miscellaneous
-------------

### hop.XMLCompile( node [, ofile] [, backend] ) ###

Compile a XML `node` into HTML. If no output file is specified,
the product of the compilation is returned in a buffer. The
optional `backend` argument is string. It denotes the HTML version to be
used for the compilation.

${ <span class="label label-info">Example:</span> }

```hopscript
var node = <html><div onclick=~{alert( "clicked" )}>click me</div></html>
console.log( hop.XMLCompile( node, false, "html5" ) );
```

${ <span class="label label-warning">Note:</span> }
explicit compilation to HTML using `hop.XMLCompile` is unncessary
for service responses. Services can directly return XML objects
in response to HTTP requests.
[:@warning]
