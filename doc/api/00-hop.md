${ var doc = require( "hopdoc" ) }

Hop
===

This module contains utilities for getting, controlling, and using the
Hop server.
The module defines functions to craft service responses, and a
broadcast function that lets a server send events to registered
remote clients.

The module also defines an API to invoke third party WebServices.

Use `require( 'hop' )` to use this module.


Server Information
------------------

The server properties defined below are read-only.


### hop.port ###
[:@glyphicon glyphicon-tag parameter]  
The port number of the running Hop server. To set the port and
protocol for the Hop server, see [config](config.html).

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

```hopscript
service getXml() {
  return hop.HTTPResponseString(
    "This resource does not exist here!",
    { startLine: "HTTP/1.0 404 File not found" } ) 
```

### hop.HTTPREsponseJson( object ) ###
[:@glyphicon glyphicon-tag function]

This convenience function returns an `\[application/json\]` value from a
JavaScript object. It is the same as:

```hopscript
hop.HTTPResponseString( JSON.stringify( obj ), { contentType: 'application/json' } )
```

### hop.HTTPResponseFile( path, [option] ) ###
[:@glyphicon glyphicon-tag function]

This class is used to respond files to clients. The argument `path` is
the full path of a existing file.

#### Example ####

${ doc.include( doc.ROOT + "/examples/file/README.md" ) }

${ <span class="label label-info">file/file.js</span> }

```hopscript
${ doc.include( doc.ROOT + "/examples/file/file.js", 14 ) }
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
${ doc.include( doc.ROOT + "/examples/authentication/authentication.js", 14 ) }
```

### hop.HTTPResponseError( obj, [option] ) ###

Respond an error value to the client, which either invokes the `fail`
callback of the `post` service call, or raises an exception.

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
${ doc.include( doc.ROOT + "/examples/svc3/svc3.js", 14 ) }
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

Invoking the `resolve` function actually sends the responds to the client.
Invoking the `reject` as the same effect of responding a `HHTPResponseError`
value.


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
${ doc.include( doc.ROOT + "/examples/image/image.js", 14 ) }
```

Broadcast
---------

Broadcast is an abstraction on top of webSockets to let a Hop server
send events to connected clients (either web browsers or Hop client
processes). Connections  originate from the client to the server,
so broadcast can be used even in the  asymetric web topology where
clients most often lie behind a NAT router or firewall and would not
accept a connection from a remote server (forbidding the remote server
to invoke services running on the client process).

### hop.broadcast( eventName, value ) ###
[:@glyphicon glyphicon-tag function]

Generates an event of type `eventName` with payload `value`. The event
is broadcast over the network to all registered clients. `eventName`
is cast into a String, `value`can be any serializable object,
including JavaScript objects, Hop.js services, and
xml-elements. Clients register to specific broadcast events with the
`addEventListener`method.

```hopscript
hop.broadcast( 'refreshScore', 14 );
```

### addEventListener( eventName, handler [, options] ) ###
[:@glyphicon glyphicon-tag function]

Use this method on the client side to register to the `eventName`
server event. The effect of this method is to establish a persistent
connection with the Hop server, register the client for the given
event type, and trigger the handler whenever the event is received by
the client. `handler` takes one argument, the event. The transmitted
`value` can be retrieved in the `value` property of the event.

When used within a web browser, connection is established with the Hop 
server serving the current page, the exact syntax is 
`server.addEventListener( eventName, handler )` where `server` denotes 
the current server (the runtime system automatically binds the 
`server` variable to the current server). 


```hopscript
server.addEventListener( 'refreshScore', function( event ) {
  var score = event.value;
  var scoreElement = this.document.getElementById( 'score' );
  // update GUI element with new score
```

The `addEventListener` method is not supported by client Hop processes.



WebService
----------

WebService is a set of API that let you invoke third party WebServices
the same way you invoke Hop services.

```hopscript
var hop = require( 'hop' );
var mymemory = hop.webService( "http://mymemory.translated.net/api/get" );
mymemory( {q: 'My tailor is rich.', langpair: 'en|fr' } ).post( function( result ) {
   console.log( result.responseData );
   }, { fail: function( error ) {
   console.log( 'failure' );
} });
```

###hop.webService( url ) ###
[:@glyphicon glyphicon-tag function]

Use this method to declare a remote WebService,that can later be
invoked with named arguments. `url`is the url of the WebService.
Call the returned function with an object argument containing the
named arguments you want to send to the WebService. The returned value
is a WebServiceFrame (very similar in use to Service Frames).


###WebServiceFrame.post([ success, [ fail-or-options ]] ) ###
[:@glyphicon glyphicon-tag function]

Invokes asynchronously the webService. The optional `success`argument,
when provided, must be a function of one argument, which is set the
the value returned by the WebService.

if the optional argument `fail-or-options` is a procedure, it is
invoked if an error occurs during the WebService invocation. If
`fail-or-options` is an object, it contains optional parameters to the
WebService invocation.

###WebServiceFrame.postSync([ success, [ fail-or-option ]] ) ###
[:@glyphicon glyphicon-tag function]

The synchronous version of `post`. Returns the value returned by the
service. Since `postSync` blocks the execution of the client process
until the service returns a value, it is strongly advised to use
the asynchronous `post` when applicable.


Miscellaneous
-------------

### hop.charsetConvert() ###
[:@glyphicon glyphicon-tag function]

### hop.Cons() ###
[:@glyphicon glyphicon-tag function]

This function is a constructor to create native (Bigloo) objects. 

### hop.encodeURIComponent() ###
[:@glyphicon glyphicon-tag function]

### hop.List() ###
[:@glyphicon glyphicon-tag function]

This function is a constructor to create native (Bigloo) objects. 

### hop.md5sum() ###
[:@glyphicon glyphicon-tag function]

### hop.sha1sum() ###
[:@glyphicon glyphicon-tag function]

### hop.signal() ###
[:@glyphicon glyphicon-tag function]

### hop.XMLCompile( node [, ofile] [, backend] ) ###
[:@glyphicon glyphicon-tag function]

Compile a XML `node` into HTML. If no output file is specified,
the product of the compilation is returned in a buffer. The 
optional `backend` argument is a string denoting the HTML version to be 
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

Sub Modules
----------

The following properties lead to sub modules that can be loaded using
the `require` function.

```hopscript
var hop = require( 'hop' );
var config = require( hop.config );
```

### hop.config ###
[:@glyphicon glyphicon-tag parameter]

See [config](config.html).

### hop.fontifier ###
[:@glyphicon glyphicon-tag parameter]


### hop.markdown ###
[:@glyphicon glyphicon-tag parameter]

See [markdown](markdown.html).

### hop.notepad ###
[:@glyphicon glyphicon-tag parameter]

### hop.security ###
[:@glyphicon glyphicon-tag parameter]

### hop.spage ###
[:@glyphicon glyphicon-tag parameter]

See [spage](spage.html).

### hop.tree ###
[:@glyphicon glyphicon-tag parameter]

See [tree](tree.html).
### hop.user ###
[:@glyphicon glyphicon-tag parameter]

See [user](user.html).

### hop.wiki ###
[:@glyphicon glyphicon-tag parameter]
