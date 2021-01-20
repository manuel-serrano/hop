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


General configuration
---------------------

### hop.engine ###
[:@glyphicon glyphicon-tag parameter]
The `engine` property is used to distinguish Hop from other JavaScript
engines when an application is compatible with different systems. With
Hop, its value is always the string `hop`.

### hop.isServer ###
[:@glyphicon glyphicon-tag parameter]
The `isServer` property is true for code executing on a server and false
for code executing on a client.


### hop.isWorker ###
[:@glyphicon glyphicon-tag parameter]
The `isWorker` property is true if and only if the expression is evaluated
within a worker context.


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

### hop.ports ###
[:@glyphicon glyphicon-tag parameter]  
Returns all the ports number of the running Hop server.


```hopscript
console.log( "port:", hop.ports );
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

### hop.arch ###
[:@glyphicon glyphicon-tag parameter]  
The Hop architecture. 

```hopscript
console.log( "Hop arch:", hop.arch );
```

### hop.loginCookieCryptKey ###
[:@glyphicon glyphicon-tag parameter]  
An unique integer seed to for password encryption. This value is
shared amongst all Hop workers.

```hopscript
console.log( "seed:", hop.loginCookieCryptKey );
```


Server Configuration
--------------------

### hop.httpAuthenticationMethod ###
[:@glyphicon glyphicon-tag parameter]  
The Hop HTTP authentication method. Can either be `"basic"` or `"digest"`.

```hopscript
console.log( "method:", hop.httpAuthenticationMethod );
```

### hop.useProxy ###
[:@glyphicon glyphicon-tag parameter]  

Proxy to be used to access internet resources.

```hopscript
hop.useProxy = "192.168.3.4";
```

### hop.enableProxying ###
[:@glyphicon glyphicon-tag parameter]  

Enable/disable Hop to act as an HTTP proxy.

```hopscript
hop.enableProxying = false;
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
 In normal situations, it is not necessary to explicitly build the
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
 In normal situations, it is not necessary to explicitly build the
`HTTPResponseXml` object as the runtime system automatically constructs
one when the response of a service is an XML fragment. It might be
useful to construct an `HTTPResponseXML` explicitly when a header
is to be associated with the response. Example:
[:@warning]

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
the full path of a existing file. The option is an object whose fields can
be:

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

Requests
--------
[:requests]

The `this` value of service invokations is an object that denotes the
current request.

### hop.isLocalRequest( req ) ###
[:@glyphicon glyphicon-tag function]

Returns `true` if and only if `req` denotes a locate request, i.e., an
http request emitted from the same machine that runs the server. Returns
`false` otherwise.


Request Filtering
-----------------
[:request-filters]

Hop [services](01-service.html) are associated to URL and the server
automatically routes requests toward the corresponding service. This
mechanism constitutes the basis of Hop web programming and in most
situtations, this high-level programming should be sufficient and
low-level details such as the configuration of the underlying socket
or the details of the `http` request attributes could be
ignored. However, in some other situations a finer control over the
connections and requests is needed.  If this only consists in
obtaining an information about an `http` connection, the reification
of the request the service receives as its `this` argument should be
enough. If this consists in obtaining information independant of
services or if this consists in answering a request that is not even
associated with a service another programming level is needed. This is
acheived by the request filters described in this section.

${ <span class="label label-warning">Note:</span> }
 Request filters can only be defined before the first request is received.
Adding filter in the `hoprc.js` file is then recommended.
[:@warning]

### hop.addRequestFilter( filter ) ###
[:@glyphicon glyphicon-tag function]  
### hop.addRequestFilterFirst( filter ) ###
[:@glyphicon glyphicon-tag function]  
### hop.addRequestFilterLast( filter ) ###
[:@glyphicon glyphicon-tag function]  

The function `hop.addRequestFilter( filter )` adds a filter, a
function of one argument, that will be invoked upon every request
received by the server.  The function `hop.addRequestFilterFirst(
filter )` adds a filter that is executed before the existing
filter. The function `hop.addRequestFilterLast( filter )` adds a
filter after the registered filters. All filters are executed in the
main worker. Their execution should then be short.

Request filters are chained together and this chaining drives the
server behavior.  If the value returned from a filter is a response
(e.g., `HTTPReponseHop`, `HTTPResponseString`, etc.), the server uses
this value to respond to the request. If the value is not a response,
then the next filter is executed.  This process repeats until the a
filter returns a response.

#### Example ####

${ doc.include( doc.BUILDDIR + "/examples/image/README.md" ) }

${ <span class="label label-info">reqfilter.js</span> }

```hopscript
${ doc.include( doc.BUILDDIR + "/doc/api/reqfilter.js" ) }
```


Server 
------

Server objects denotes a remote server. They are used to attached
listeners to server events (see [Broadcast](00-hop.html#broadcast))
and to invoke services from server to another server (see
[service](01-service.html)).


### new hop.Server( [ hostname [, port [, authorization [, ssl ] ] ] ) ###
[:server@glyphicon glyphicon-tag constructor]

Creates a new server object. The arguments are as follows:

  * `hostname`: a string, the name or IP number of the remote host that
  will emit signals. If omitted, defaults to the running host name.
  * `port`: the port number of the remote host. If omitted, defaults to
  the running Hop port.
  * `authorization`: a string, an optional authorization for accessing
  the remote host. This has the syntax of the
  frame `[post](01-service.html#post)` method.
  * `ssl`: a optional boolean. When true, the established channel between
  the two servers uses SSL.
  

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

Two predefined events are automatically sent to clients:

  * `ready`: that event is emitted when a new listener is attached to a server.
  * `down`: that event is emtted when the connection with the server is lost.
  

### Server.removeEventListener( eventName, handler ) ###
[:@glyphicon glyphicon-tag function]

Removes an attached listener.


Broadcast
---------
[:broadcast]

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


EventMonitor
------------

Event listener monitors are used to react to client connection requests.

### hop.eventListenerMonitor( eventName ) ###
[:@glyphicon glyphicon-tag constructor]

Creates a new event monitor on event `eventName`.

### eventListenerMonitor.monitor( eventName ) ###
[:@glyphicon glyphicon-tag function]

Add a new event to be monitored by this monitor.


### eventListenerMonitor.addEventListener( event, callback ) ###
[:@glyphicon glyphicon-tag function]

The argument `event` can be `newListener` or `removeListener`:

  * `newListener`: the associated callback will be invoked each time a 
client will register a listener on event `eventName`, the event name 
used to build the monitor.
  * `removeListener`: the associated callback will be invoked on 
client deconnection.

Example:


```hopscript
${ doc.include( doc.BUILDDIR + "/examples/evtmonitor/evtserver.js", 14 ) }
```


Web Service
-----------

A WebService reifies an API, i.e., a set of services, that let you
invoke third party WebServices the same way you invoke Hop services
(see [Interoperable WebServices](01-service.html#interop) for
interoperability notes).


```hopscript
var hop = require( 'hop' );
var mymemory = hop.webService( "http://mymemory.translated.net/api/get" );
mymemory( {q: 'My tailor is rich.', langpair: 'en|fr' } ).post( function( result ) {
   console.log( result.responseData );
   }, { fail: function( error ) {
   console.log( 'failure' );
} });
```

### hop.webService( url ) ###
[:@glyphicon glyphicon-tag function]

Use this method to declare a remote WebService,that can later be
invoked with named arguments. `url`is the url of the WebService.
Call the returned function with an object argument containing the
named arguments you want to send to the WebService. The returned value
is a WebServiceFrame (very similar in use to Service Frames).


### WebServiceFrame.post([ success [, fail-or-options]] ) ###
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

### WebServiceFrame.postSync([ success [, fail-or-option]] ) ###
[:@glyphicon glyphicon-tag function]

The synchronous version of `post`. Returns the value returned by the
service. Since `postSync` blocks the execution of the client process
until the service returns a value, it is strongly advised to use
the asynchronous `post` when applicable. The options are shared with the
`post` method.


Compiler Driver
---------------

The compiler driver provides information about the background compilation
status.

### hop.compilerDriver ###
[:@glyphicon glyphicon-tag parameter]

### hop.compilerDriver.policy ###
[:@glyphicon glyphicon-tag parameter]

The compilation policy. The possible values are:

  * `none`: never compile.
  * `aot`: compile before loading.
  * `nte`: compile for the next execution. The pending compilation are stopped
 when the current execution is about to end.  
  * `nte+`: compile for the next execution and wait for all running compilations
 before ending current execution.


### hop.compilerDriver.pending ###
[:@glyphicon glyphicon-tag parameter]

The number of pending background compilations.

### hop.compilerDriver.addEventListener( eventName, handler [, options] ) ###
[:@glyphicon glyphicon-tag function]

This method is used to add an listener to the compiler driver. The known
events are

  * `start`: emitted when a background compilation starts.
    The `event.target` denotes the file name.
  * `end`: emitted when a background compilation ends.
    The `event.target` denotes the file name and `event.value` the
    compilation termination status (an integer).
  * `all`: emitted when all background compilation complete. This event
    fires, when the listener is added and when there is no pending
    compilation.

### hop.compilerDrive.removeEventListener( eventName, handler ) ###
[:@glyphicon glyphicon-tag function]

Removes an attached listener.


Miscellaneous
-------------

### hop.charsetConvert( text, source, target ) ###
[:@glyphicon glyphicon-tag function]

Converts the `text` string from charset `source` into charset `target`.

${ <span class="label label-info">url/url.js</span> }

```hopscript
${ doc.include( doc.BUILDDIR + "/examples/url/url.js", 14 ) }
```

### hop.decodeHTML( string ) ###
[:@glyphicon glyphicon-tag function]

Decodes an encoded HTML string.

```hopscript
hop.decodeHTML( 'jean &lt;dupont&gt;' );
// "jean <dupont>"
```

### hop.decodeURIComponent( string ) ###
[:@glyphicon glyphicon-tag function]

Decodes an encoded URI component.

```hopscript
hop.encodeURIComponent( 'jean dupont' );
// "jean%20dupont"
```

### hop.encodeHTML( string ) ###
[:@glyphicon glyphicon-tag function]

Encodes an HTML string into a textual string.

```hopscript
hop.encodeHTML( 'jean <dupont>' );
// "jean &lt;upont&gt;"
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

### hop.base64encode( string ) ###
[:@glyphicon glyphicon-tag function]

Encodes a string into base64.

### hop.base64decode( string ) ###
[:@glyphicon glyphicon-tag function]

Decodes a base64 string.


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

Hop configutation. See [config](config.html).

### hop.csv ###
[:@glyphicon glyphicon-tag parameter]

Efficient CSV parser. See [csv](csv.html).

### hop.feed ###
[:@glyphicon glyphicon-tag parameter]

RSS manipulation. See [feed](feed.html).

### hop.fontifier ###
[:@glyphicon glyphicon-tag parameter]

### hop.hss ###
[:@glyphicon glyphicon-tag parameter]

See [hss](hss.html).

### hop.hopc ###
[:@glyphicon glyphicon-tag parameter]

API for deadling with the Hop compiler. See [hopc](hopc.html).

### hop.hopdroid ###
[:@glyphicon glyphicon-tag parameter]

Hop Android environment. See [hopdroid](hopdroid.html).

### hop.markdown ###
[:@glyphicon glyphicon-tag parameter]

Home brewed markdown parser. See [markdown](markdown.html).

### hop.notepad ###
[:@glyphicon glyphicon-tag parameter]

XML widget.

### hop.security ###
[:@glyphicon glyphicon-tag parameter]

### hop.spage ###
[:@glyphicon glyphicon-tag parameter]

XML widget. See [spage](spage.html).

### hop.syslog ###
[:@glyphicon glyphicon-tag parameter]

Unix syslog. See [syslog](syslog.html).

### hop.systime ###
[:@glyphicon glyphicon-tag parameter]

Execution time. See [systime](systime.html).

### hop.system ###
[:@glyphicon glyphicon-tag parameter]

Unix system like command. See [system](system.html).

### hop.tree ###
[:@glyphicon glyphicon-tag parameter]

### hop.texinfo ###
[:@glyphicon glyphicon-tag parameter]

See [texinfo](texinfo.html).

See [tree](tree.html).
### hop.user ###
[:@glyphicon glyphicon-tag parameter]

XML widget. See [user](user.html).

### hop.vcf ###
[:@glyphicon glyphicon-tag parameter]

VCards and other contact formats. See [vcf](vcf.html).

### hop.wiki ###
[:@glyphicon glyphicon-tag parameter]

Hop user declaration and manipulation. See [tree](tree.html).

### hop.xml ###
[:@glyphicon glyphicon-tag parameter]

See [xml](xml.html).

