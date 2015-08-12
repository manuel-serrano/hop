${ var doc = require( "hopdoc" ) }

Hop
===

This module contains utilities for getting, controlling, and using the
Hop server.

Use `require( 'hop' )` to use this module.


Configuration
-------------

### hop.shareDir ###
[:@glyphicon glyphicon-tag]
The `share` directory of the Hop installation.

```hopscript
console.log( "share:", hop.shareDir );
```

### hop.binDir ###
[:@glyphicon glyphicon-tag]
The `bin` directory of the Hop installation.

```hopscript
console.log( "bin:", hop.binDir );
```

### hop.libDir ###
[:@glyphicon glyphicon-tag]
The `lib` directory of the Hop installation.

```hopscript
console.log( "lib:", hop.libDir );
```

### hop.port ###
[:@glyphicon glyphicon-tag]
The port number of the running Hop server.

```hopscript
console.log( "port:", hop.port );
```

### hop.hostname ###
[:@glyphicon glyphicon-tag]
The host name of the running Hop server.

```hopscript
console.log( "hostname:", hop.hostname );
```


### hop.version ###
[:@glyphicon glyphicon-tag]
The Hop version.

```hopscript
console.log( "Hop version:", hop.version );
```


Responses
---------

### new hop.HTTPResponseHop( obj, [option] ) ###
[:@glyphicon glyphicon-tags]

This class is used to respond values to client requests.

Example:
```hopscript
service getObj() {
  return new hop.HTTPResponseHop( { key: "foo", value: [ 1,2 3 ] } );
```

${ <span class="label label-warning">Note:</span> }
 In normal situation, it is not necessary to explicitly build the
`HTTPResponseHop` object as the runtime system automatically constructs
one when the response of a service is a compound JavaScript object.
[:@warning]


### hop.HTTPResponseXml ###
[:@glyphicon glyphicon-tags]

This class is used to deliver XML documents to client. 

Example:
```hopscript
service getXml() {
  return new hop.HTTPResponseXML( <div>a div</div> );
```

${ <span class="label label-warning">Note:</span> }
 In normal situation, it is not necessary to explicitly build the
`HTTPResponseXML` object as the runtime system automatically constructs
one when the response of a service is an XML fragment.
[:@warning]


### hop.HTTPResponseString ###
[:@glyphicon glyphicon-tags]

This class is used to deliver plain character strings to client.

Example:
```hopscript
service getXml() {
  return new hop.HTTPResponseString(
    "This resource does not exist here!",
    { startLine: "HTTP/1.0 404 File not found" } ) 
```

### new hop.HTTPResponseFile( path, [option] ) ###
[:@glyphicon glyphicon-tags]

This class is used to respond files to clients. The argument `path` is
the full path of a existing file.

#### Example ####

${ doc.include( doc.ROOT + "/examples/async/doc.md" ) }

${ <span class="label label-info">file/file.js</span> }

```hopscript
${ doc.include( doc.ROOT + "/examples/file/file.js", 13 ) }
```

${ <span class="label label-warning">Note:</span> }
 HTTPResponseFile is a much faster way to send a file to a client, althought,
the same behaviour can also be implemented combining standard `fs` operations
and `HTTPResponseString` values.
[:@warning]

### new hop.HTTPResponseAuthentication( msg, [option] ) ###
[:@glyphicon glyphicon-tags]

This class is used to respond HTTP `401 Unauthorized` response to Web
client.

${ <span class="label label-warning">Note:</span> }
 the class `hop.HTTPResponseAuthentication` is a convenience class.
The same behavior can be implemented using `hop.HTTPResponseString`
and passing a `tagstLine` value in the optional argument.
[:@warning]

#### Example ####

${ doc.include( doc.ROOT + "/examples/authentication/doc.md" ) }

${ <span class="label label-info">authentication/authentication.js</span> }

```hopscript
${ doc.include( doc.ROOT + "/examples/authentication/authentication.js", 13 ) }
```

### hop.HTTPResponseAsync ###
[:@glyphicon glyphicon-tags]

#### Example ####

${ doc.include( doc.ROOT + "/examples/async/doc.md" ) }

${ <span class="label label-info">async/async.js</span> }

```hopscript
${ doc.include( doc.ROOT + "/examples/async/async.js", 13 ) }
```

### hop.HTTPResponseProxy ###
[:@glyphicon glyphicon-tags]

Miscellaneous
-------------

### hop.XMLCompile( node [, ofile] [, backend] ) ###
[:@glyphicon glyphicon-tags]

Compile a XML `node` into HTML. If no output file is specified,
the product of the compilation is returned in a buffer. The
optional `backend` argument is string. It denotes the HTML version to be
used for the compilation.

```hopscript
var node = <html><div onclick=~{alert( "clicked" )}>click me</div></html>
console.log( hop.XMLCompile( node, false, "html5" ) );
```

${ <span class="label label-warning">Note:</span> }
 explicit compilation to HTML using `hop.XMLCompile` is unncessary
for service responses. Services can directly return XML objects
in response to HTTP requests.
[:@warning]
