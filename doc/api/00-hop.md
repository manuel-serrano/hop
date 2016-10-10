${ var doc = require( "hopdoc" ) }

Hop
===

This module contains utilities for getting, controlling, and using the
Hop server.  The module defines functions to craft service responses,
and a broadcast function that lets a server send events to registered
remote clients.

The module also defines an API to invoke third party Web Services.

For ease of use, `hop` is defined as a global object and can be used
directly without require.


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

The options list is:

  * `startLine`: a string denoting the HTTP start line.
  * `contentType`: the `content-type` of the response.
  * `charset`: the charset.
  * `header`: the full response header, an object.


### hop.HTTPResponseXml( obj, [option] ) ###
[:@glyphicon glyphicon-tag function]

This class is used to deliver XML documents to client. 

```hopscript
service getXml() {
  return hop.HTTPResponseXml( <div>a div</div> );
```

The options list is:

  * `backend`: the HTML backend (defaults to "HTML5")
  * `startLine`: a string denoting the HTTP start line.
  * `contentType`: the `content-type` of the response.
  * `charset`: the charset.
  * `header`: the full response header, an object.

${ <span class="label label-warning">Note:</span> }
 In normal situation, it is not necessary to explicitly build the
`HTTPResponseXml` object as the runtime system automatically constructs
one when the response of a service is an XML fragment. It might be
useful to construct an `HTTPResponseXML` explicitly when a header
is to be associated with the response. Example:

```hopscript
service foo() {
   return hop.HTTPResponseXml(
     <html>
       <button onclick=~{console.log( document.cookie )}>show</button>
     </html>,
     { contentType: "text/html", header: { "set-cookie": "a=b; HttpOnly" } } );
}
```
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

The options list is:

  * `startLine`: a string denoting the HTTP start line.
  * `contentType`: the `content-type` of the response.
  * `charset`: the charset.
  * `header`: the full response header, an object.


### hop.HTTPResponseJson( object ) ###
[:@glyphicon glyphicon-tag function]

This convenience function returns an `\[application/json\]` value from a
JavaScript object. It is the same as:

```hopscript
hop.HTTPResponseString( JSON.stringify( obj ), { contentType: 'application/json' } )
```

  * `startLine`: a string denoting the HTTP start line.
  * `contentType`: the `content-type` of the response.
  * `charset`: the charset.
  * `header`: the full response header, an object.


### hop.HTTPResponseFile( path, [option] ) ###
[:@glyphicon glyphicon-tag function]

This class is used to respond files to clients. The argument `path` is
the full path of a existing file.

  * `contentType`: the `content-type` of the response.
  * `charset`: the charset.
  * `header`: the full response header, an object.


#### Example ####

${ doc.include( doc.BUILDDIR + "/examples/file/README.md" ) }

${ <span class="label label-info">file/file.js</span> }

```hopscript
${ doc.include( doc.BUILDDIR + "/examples/file/file.js", 14 ) }
```

${ <span class="label label-warning">Note:</span> }
 HTTPResponseFile is a much faster way to send a file to a client, althought,
the same behaviour can also be implemented combining standard `fs` operations
and `HTTPResponseString` values.
[:@warning]

### hop.HTTPResponseAuthentication( msg, [request] ) ###
[:@glyphicon glyphicon-tag function]

This class is used to respond HTTP `401 Unauthorized` response to Web
client.

${ <span class="label label-warning">Note:</span> }
 the class `hop.HTTPResponseAuthentication` is a convenience class.
The same behavior can be implemented using `hop.HTTPResponseString`
and passing a `startLine` value in the optional argument.
[:@warning]



#### Example ####

${ doc.include( doc.BUILDDIR + "/examples/authentication/README.md" ) }

${ <span class="label label-info">authentication/authentication.js</span> }

```hopscript
${ doc.include( doc.BUILDDIR + "/examples/authentication/authentication.js", 14 ) }
```

### hop.HTTPResponseError( obj ) ###

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
 * The argument `req` is either a request object (the `this` value of
 the service invokation) or an object containing the optional fields.
  * `currentRequest`: the request object.
  * `contentType`: the `content-type` of the response.
  * `charset`: the charset.
  * `header`: the full response header, an object.


#### Example ####

${ doc.include( doc.BUILDDIR + "/examples/svc3/README.md" ) }

${ <span class="label label-info">svc3/svc3.js</span> }

```hopscript
${ doc.include( doc.BUILDDIR + "/examples/svc3/svc3.js", 14 ) }
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


### hop.HTTPResponseProxy( obj ) ###
[:@glyphicon glyphicon-tag function]
 
The `hop.HTTPResponseProxy` objects are to be used when a remote resource
can be access othwerwise. For instance, these situations arise because of
the security enforcement of the Web browsers. Some resources have to be
downloaded from the origin server. Using a `hop.HTTPResponseProxy` object
enables the web page to only use local URLs, that are proxied to the actual
remote resources by the server.

#### Example ####

${ doc.include( doc.BUILDDIR + "/examples/image/README.md" ) }

${ <span class="label label-info">image/image.js</span> }

```hopscript
${ doc.include( doc.BUILDDIR + "/examples/image/image.js", 14 ) }
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

### hop.signal() ###
[:@glyphicon glyphicon-tag function]

This function is similar to `broadcast` but only one receiver will be
notified of the message.

### Server.addEventListener( eventName, handler [, options] ) ###
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

On the server side, server objects are instances of the Server class.

### new hop.Server( [ hostname [, port [, authorization [, ssl ] ] ] ) ###
[:server@glyphicon glyphicon-tag constructor]

the arguments are as follows:

  * `hostname`: a string, the name or IP number of the remote host that
  will emit signals. If omitted, defaults to the running host name.
  * `port`: the port number of the remote host. If omitted, defaults to
  the running Hop port.
  * `authorization`: a string, an optional authorization for accessing
  the remote host. This has the syntax of the
  frame `[post](01-service.html#post)` method.
  * `ssl`: a optional boolean. When true, the established channel between
  the two servers uses SSL.
  
  
```hopscript
var srv = new hop.Server( "localhost", 9999 );

srv.addEventListener( 'refreshScore', function( event ) {
   var score = event.value;
   ...
} )

service getScore();
getScore.call( srv, "jean dupont" ).post( v => ... );
```


Web Service
-----------

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


###WebServiceFrame.post([ success [, fail-or-options]] ) ###
[:@glyphicon glyphicon-tag function]

Invokes asynchronously the webService. The optional `success`argument,
when provided, must be a function of one argument, which is set the
the value returned by the WebService.

if the optional argument `fail-or-options` is a procedure, it is
invoked if an error occurs during the WebService invocation. If
`fail-or-options` is an object, it contains optional parameters to the
WebService invocation.

The list of valid options are:

  * `hostname`: the remote host.
  * `port`: the remote host port.
  * `authorization`: a identification of the form `name:password`.
  * `fail`: a failure callback.
  * `scheme`: the scheme used for the request (defaults to `http`).
  * `ssl`: a boolean to enable secure connections.
  * `timeout`: a number of milliseconds.
  * `method`: the method of the call (e.g., `GET` or `POST`).
  * `header`: the complete header of the request. The header is an
  regular object.
  * `body`: a string, denoting the body of the request.

Example:

```hopscript
var ws = hop.webService( "http://localhost:1337/api/oauth/token" );

ws()
  .postSync(
   { method: "POST",
    header: { "content-type": "application/x-www-form-urlencoded" },
    body: "grant_type=password&client_id=android&client_secret=SomeRandomCharsAndNumbers&username=myapi&password=abc1234" } );
```

###WebServiceFrame.postSync([ success [, fail-or-option]] ) ###
[:@glyphicon glyphicon-tag function]

The synchronous version of `post`. Returns the value returned by the
service. Since `postSync` blocks the execution of the client process
until the service returns a value, it is strongly advised to use
the asynchronous `post` when applicable. The options are shared with the
`post` method.


Miscellaneous
-------------

### hop.charsetConvert( text, source, target ) ###
[:@glyphicon glyphicon-tag function]

Converts the `text` string from charset `source` into charset `target`.

${ <span class="label label-info">url/url.js</span> }

```hopscript
${ doc.include( doc.BUILDDIR + "/examples/url/url.js", 14 ) }
```

### hop.encodeURIComponent( string ) ###
[:@glyphicon glyphicon-tag function]

Encodes a string into a valid URI component.

```hopscript
hop.encodeURIComponent( 'jean dupont' );
// "jean%20dupont"
```

### hop.Cons() ###
[:@glyphicon glyphicon-tag function]

This function is a constructor to create native (Bigloo) objects. 

### hop.List() ###
[:@glyphicon glyphicon-tag function]

This function is a constructor to create native (Bigloo) objects. 

### hop.md5sum( string ) ###
[:@glyphicon glyphicon-tag function]

Computes the md5sum of a string.

```hopscript
hop.md5sum( 'jean dupont' );
// "b38bed581de7b86dd6fc8355c73cebf2"
```

### hop.sha1sum( string ) ###
[:@glyphicon glyphicon-tag function]

Computes the sha1 sum of a string.

```hopscript
hop.sha1sum( 'jean dupont' );
// "7461340811509ec24dd1c1a32504a01e24423768"
```

### hop.compileXML( node [, ofile] [, backend] ) ###
[:@glyphicon glyphicon-tag function]

Compile a XML `node` into HTML. If no output file is specified,
the product of the compilation is returned in a buffer. The 
optional `backend` argument is a string denoting the HTML version to be 
used for the compilation. 

```hopscript
var node = <html><div onclick=~{alert( "clicked" )}>click me</div></html>
console.log( hop.compileXML( node, false, "html5" ) );
```

${ <span class="label label-warning">Note:</span> }
 explicit compilation to HTML using `hop.compileXML` is unncessary
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
