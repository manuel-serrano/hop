${ var doc = require( "hopdoc" ) }
${ var fontifier = require( "fontifier" ) }
${ var path = require( "path" ) }
${ var ROOT = path.dirname( module.filename ) }

HopScript Service
=================

A Hop.js service is a function that is that is callable through the
network.  The service is declared on the server side, and invoked from
a Hop.js client process, a Hop.js application running on a web
browser, or a third party application (services are built on top of
HTTP, they can be invoked using the hop.js API or from handcrafted GET
and POST HTTP requests).

Invoking a service builds a _service frame_. This frame can
be used to actually invoke the service. If the service declaration
used _named arguments_ the frame can be automatically built out of a
standard URI specified in [RFC3986](https://tools.ietf.org/html/rfc3986).

Example:

${ <span class="label label-warning">TODO</span> } Je pense que
l'exemple est trop complexe our une introduction. Risque de confusion entre
service et webservice.  [:@warning]


${ <span class="label label-info">url/url.js</span> }

```hopscript
${ doc.include( doc.EXAMPLES_DIR + "/url/url.js", 13 ) }
```


Service Declarations
--------------------

The syntax of service declarations is as follows:

```ebnf
${ doc.include( ROOT + "/service.bnf" ) }
```

Examples:

```hopscript
function checkDB( fname, lname ) {
   return (fname + ":" + lname) in DB;
}
service svc1( fname, lname ) { return checkDB( fname, lname ) }
service svc2( {fname: "jean", lname: "dupond"} ) { return checkDB( fname, lname ) }
```

Services have a lot in common with ordinary functions, they can be
declared in statements, or  within expressions. Service expressions
can be named or anonymous.

Services can be invoked as soon as they are declared.

Service arguments are handled like in a function declaration, missing
arguments are undefined in the function body, extra arguments are
ignored, and the `arguments`object contains all the arguments passed
to the service invocation.

Contrary to functions, services can also be declared with an object literal
in place of the formal parameter list. This special form has two
purposes: supporting RFC3986 compliant requests (keys correspond to
the URI keys), and providing default values for missing arguments.


${ <span class="label label-warning">Note:</span> } When used within a
service declaration, `this` is associated with the runtime request
object corresponding to a service invocation. This object contains all
the information about the current HTTP request.  [:@warning]
 
Example:

```hopscript
service svc( { name: "durand" } ) {
  console.log( "host=", this.host, " port=", this.port );
  console.log( "abspath=", this.abspath );
  return true;
}
```
${ <span class="label label-warning">TODO</span> } quelles sont les 
propriétés de l'objet HTTP request? [:@warning]

Service are free to return any _serializable_ object. The value
is first converted into a `hop.HTTPResponse` object by Hop.js. This converted
value is sent to the client. The rules for converting values into
`hop.HTTPResponse` are as follows:

 * if the response is a string, Hop constructs a `hop.HTTPResponseString`
 object.
 * if the response is a XML document, a `hop.HTTPResponseXML` object is
 constructed.
 * If the response is a promise, a `hop.HTTPResponseAsync` object is built.
 * if the response is a JavaScript object.
   * if that object has a `toResponse` property which is a function, the
   result of invoking this function is used as a reponse.
 * Otherwise, a `hop.HTTPResponseHop` is constructed. This will have the
 effect of serializing the JavaScript object and re-creating it on the client.

The various Hop responses classes are documented [here](00-hop.html#responses).

Service Frames
--------------

Invoking a service returns a `HopFrame` object that can later spawn the
execution of the service body.

Example:

```hopscript
var frame = svc2( { lname: "durant" } );
typeof( frame );           // "object"
frame instanceof HopFrame; // true
frame.toString();          // /hop/svc2?hop-encoding=hop&vals=c%01%02(%01%0...
```

A `HopFrame` implements the methods described in the section.

### frame.post( [ success, [ fail-or-option ] ] ) ###
[:@glyphicon glyphicon-tag function]


Invokes asynchronously the service. The  optional `success` argument,
when provided, must be a function of one argument. The argument is the
value returned by the service.

Example:

```hopscript
svc2( { name: "dupond" } )
   .post( function( r ) { console.log( r ); } );
```

If the optional argument `fail-or-option` is a procedure, it is invoked
if an error occurs while invoking the service. If `fail-or-option` is
an object, here are the attributes this object may contain:

 * `host`, the host name on which the service is invoked. This option is
only used when invoking service from servers to servers, and defaults
to `hop.host`.
 * `port`, the port number of the remote host. This option is
only used when invoking service from servers to servers, and defaults
to `hop.port`.
 * `user`, a user identity on behalf of who the service is invoked.
 * `password`, the user password.
 * `fail`, a failure procedure.
 * `scheme`, the schema to be used to invoke the service. Should normally be
either `http` or `https`. Defaults to `http`.
 * `ssl`, a boolean which specifies if SSL is to be used to invoke the
   service. Defaults to `false`.
 * `header`, a JavaScript object to add properties to the HTTP header of the request.

Example:

```hopscript
var config = {
  host: "remote.org",
  port: 443,
  schema: "https",
  ssl: true,
  header: { "X-encoding", "my-encoding" }
};

svc2( { name: "dupond" } )
   .post( function( r ) { console.log( r ); }, config );

```
### frame.postSync( [ fail-or-option ]  ) ###
[:@glyphicon glyphicon-tag function]

The synchronous version of `post`. Returns the value returned by the
service. Since `postSync`blocks the execution of the client process
until the service returns a value, it is strongly advised to use the
asynchronous version of `post`instead.


### HopFrame as URLs ###

HopFrame can be everywhere a URL is expected, in particular, in HTML
nodes. For instance, the `src` attribute of an image can be filled with
an HopFrame. In that case, the content of the image will be the result
of the service invocation.

Example:

```hopscript
service getImg( file ) {
  if( !file ) {
     return hop.HTTPResponseFile( DEFAULT_IMG );
  } else {
     return hop.HTTPResponseFile( ROOT + "/" + file );
  }
}

service page() {
   return <html>
      <img src=${getImg( false )}.toString()/>
      <img src=${getImg( "monalisa.jpg" )}.toString()/>
   </html>
}
```


Service Invocation
------------------

Invoking the `post` or `postSync` methods of a service frame triggers
the remote invocation of the service. That is, the arguments serialized
in the service frame are transmitted to the remote host and the service
body is executed.

Service methods & attributes
----------------------------


### service.path ###
[:@glyphicon glyphicon-tag parameter]

The path (_i.e.,_ the absolute path of the URL) of the associated service.
This string must be prefixed by `/hop/`.

Example

```hopscript
svc2.path = "/hop/dummy";
```
When a named service is declared, the default value for
`service.path`is `/hop/<service-name>`.
Anonymous services get a unique path built by hop, prefixed by
`/hop/public/`.
Changing the service path can be done at any time. A path value which
is currently assigned to a service cannot be assigned to another
service.

${ <span class="label label-warning">Note:</span> }
Services are global resources of a hop.js server. Services declared in
a [worker](./02-worker.md) cannot use an already assigned path. This
is the cost to pay to benefit from automatic routing of service
invocations to the proper worker thread.
[:@warning]


### service.resource( file ) ###
[:@glyphicon glyphicon-tag function]

Create the absolute path relatively to the file defining the service. For instance,
this can be used to obtained the absolute path of a CSS file or an image whose
name is known relatively to the source file defining the service.

#### Example ####

${ doc.include( doc.EXAMPLES_DIR + "/svc/README.md" ) }

${ <span class="label label-info">svc/svc.js</span> }

```hopscript
${ doc.include( doc.EXAMPLES_DIR + "/svc/svc.js", 13 ) }
```

### service.timeout ###
[:@glyphicon glyphicon-tag parameter]

The number of seconds the service is live. Negative values means infinite
timeout.

#### Example ####

```hopscript
console.log( svc2.timeout );
```

### service.ttl ###
[:@glyphicon glyphicon-tag parameter]

The number of time the service can be invoked. Negative values mean
infinite time-to-live.

Example

```hopscript
svc2.ttl = 5;
```

### service.unregister() ###
[:@glyphicon glyphicon-tag function]

Unregister a service from the Hop.js server. Once unregistered services
can no longer be invoked in response to client requests.

Importing Services
------------------

The implementation of a Hop service is not required to be known for
being invoked by a remote client. This client can _import_ the
service and use it as if it was locally defined. The syntax for
importing a service is as follows:

```ebnf
${ doc.include( ROOT + "/service.bnf" ) }
```

Imported services are used as locally defined service.


${ doc.include( doc.EXAMPLES_DIR + "/svc2/README.md" ) }

${ <span class="label label-info">svc2/svc2.js</span> }

```hopscript
${ doc.include( doc.EXAMPLES_DIR + "/svc2/svc2.js", 13 ) }
```

${ <span class="label label-info">svc2/extern.js</span> }

```hopscript
${ doc.include( doc.EXAMPLES_DIR + "/svc2/extern.js", 13 ) }
```
