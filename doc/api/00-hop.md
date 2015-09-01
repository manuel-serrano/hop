${ var doc = require( "hopdoc" ) }

Hop
===

This module contains utilities for getting, controlling, and using the
Hop server.

Use `require( 'hop' )` to use this module.


Configuration
-------------

### hop.shareDir ###
[:@glyphicon glyphicon-tag parameter]
The `share` directory of the Hop installation.

```hopscript
console.log( "share:", hop.shareDir );
```

### hop.binDir ###
[:@glyphicon glyphicon-tag parameter]
The `bin` directory of the Hop installation.

```hopscript
console.log( "bin:", hop.binDir );
```

### hop.libDir ###
[:@glyphicon glyphicon-tag parameter]
The `lib` directory of the Hop installation.

```hopscript
console.log( "lib:", hop.libDir );
```

### hop.modulesDir ###
[:@glyphicon glyphicon-tag parameter]
The `node_modules` directory of the Hop installation.

```hopscript
console.log( "modules:", hop.modulesDir );
```

### hop.port ###
[:@glyphicon glyphicon-tag parameter]
The port number of the running Hop server.

```hopscript
console.log( "port:", hop.port );
```

### hop.hostname ###
[:@glyphicon glyphicon-tag parameter]
The host name of the running Hop server.

```hopscript
console.log( "hostname:", hop.hostname );
```


### hop.version ###
[:@glyphicon glyphicon-tag parameter]
The Hop version.

```hopscript
console.log( "Hop version:", hop.version );
```


Responses
---------
[:responses]

Service result values are transformed into Hop *responses* before being
sent to the clients. 

### hop.HTTPResponseHop( obj, [option] ) ###
[:FOO@glyphicon glyphicon-tag function]

This class is used to respond values to client requests.

Example:
```hopscript
service getObj() {
  return hop.HTTPResponseHop( { key: "foo", value: [ 1,2 3 ] } );
```

${ <span class="label label-warning">Note:</span> }
 In normal situation, it is not necessary to explicitly build the
`HTTPResponseHop` object as the runtime system automatically constructs
one when the response of a service is a compound JavaScript object.
[:@warning]


### hop.HTTPResponseXml( obj, [option] ) ###
[:@glyphicon glyphicon-tag function]

This class is used to deliver XML documents to client. 

Example:
```hopscript
service getXml() {
  return hop.HTTPResponseXML( <div>a div</div> );
```

${ <span class="label label-warning">Note:</span> }
 In normal situation, it is not necessary to explicitly build the
`HTTPResponseXML` object as the runtime system automatically constructs
one when the response of a service is an XML fragment.
[:@warning]


### hop.HTTPResponseString( string, [option] ) ###
[:@glyphicon glyphicon-tag function]

This class is used to deliver plain character strings to client.

Example:
```hopscript
service getXml() {
  return hop.HTTPResponseString(
    "This resource does not exist here!",
    { startLine: "HTTP/1.0 404 File not found" } ) 
```

### hop.HTTPResponseFile( path, [option] ) ###
[:@glyphicon glyphicon-tag function]

This class is used to respond files to clients. The argument `path` is
the full path of a existing file.

#### Example ####

${ doc.include( doc.ROOT + "/examples/file/README.md" ) }

${ <span class="label label-info">file/file.js</span> }

```hopscript
${ doc.include( doc.ROOT + "/examples/file/file.js", 13 ) }
```

${ <span class="label label-warning">Note:</span> }
 HTTPResponseFile is a much faster way to send a file to a client, althought,
the same behaviour can also be implemented combining standard `fs` operations
and `HTTPResponseString` values.
[:@warning]

### hop.HTTPResponseAuthentication( msg, [option] ) ###
[:@glyphicon glyphicon-tag function]

This class is used to respond HTTP `401 Unauthorized` response to Web
client.

${ <span class="label label-warning">Note:</span> }
 the class `hop.HTTPResponseAuthentication` is a convenience class.
The same behavior can be implemented using `hop.HTTPResponseString`
and passing a `tagstLine` value in the optional argument.
[:@warning]

#### Example ####

${ doc.include( doc.ROOT + "/examples/authentication/README.md" ) }

${ <span class="label label-info">authentication/authentication.js</span> }

```hopscript
${ doc.include( doc.ROOT + "/examples/authentication/authentication.js", 13 ) }
```

### hop.HTTPResponseAsync( sender, req ) ###
[:@glyphicon glyphicon-tag function]

Asynchronous responses are used when a service cannot returns instantly
a value to a client because it relies on an asynchronous computation.
In that situation, the service must produce a `hop.HTTPResponseAsync` which
is interpreted by the builtin server as a delayed reply.

 * The argument `sender` is a function of one argument. This function is
automatically invoked by the runtime system with a value that is a
function of one parameter. Invoking that function provokes the delivery
of the reply to the client.
 * The argument `req` is a request object. It is the `this` value of
the service invokation.

#### Example ####

${ doc.include( doc.ROOT + "/examples/svc3/README.md" ) }

${ <span class="label label-info">svc3/svc3.js</span> }

```hopscript
${ doc.include( doc.ROOT + "/examples/svc3/svc3.js", 13 ) }
```

${ <span class="label label-warning">Note:</span> }
 the class `hop.HTTPResponseAsync` is the base class for
implementing asynchronous reponses. Returning
`Promise` objects as a similar behavior and is encouraged. The service `foo`
defined above can be implemented as:
[:@warning]

```hopscript
service foo( x ) {
   console.log( "in foo x=", x );
   return new Promise( function( resolve, reject ) {
      bar( x + 1 ).post( resolve );
   }
}
```

### hop.HTTPResponseProxy( obj, [option] ) ###
[:@glyphicon glyphicon-tag function]

The `hop.HTTPResponseProxy` objects are to be used when a remote resource
can be access othwerwise. For instance, these situations arise because of
the security enforcement of the Web browsers. Some resources have to be
downloaded from the origin server. Using a `hop.HTTPResponseProxy` object
enables the web page to only use local URLs, that are proxied to the actual
remote resources by the server.

#### Example ####

${ doc.include( doc.ROOT + "/examples/image/README.md" ) }

${ <span class="label label-info">image/image.js</span> }

```hopscript
${ doc.include( doc.ROOT + "/examples/image/image.js", 13 ) }
```


Miscellaneous
-------------

### hop.XMLCompile( node [, ofile] [, backend] ) ###
[:@glyphicon glyphicon-tag function]

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
