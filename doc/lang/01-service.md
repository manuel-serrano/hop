${ var doc = require( "hopdoc" ) }
${ var fontifier = require( "fontifier" ) }

HopScript Service
=================

A service is a function that is that is callable throught the network.
Invoking a service builds a _service frame_. This frame can be
used to actually invoke the service. If the service declaration
used _named arguments_ the frame can be automatically build out
of a standard URL.

Example:

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

### frame.postSync( [ success, [ fail-or-option ] ] ) ###
[:@glyphicon glyphicon-tag function]

Invokes synchronously the service. The argument optional `success` argument,
when provided, must be a function of one argument. The argument is the
value returned by the service.

Example:

```hopscript
svc2( { name: "dupond" } )
   .post( function( r ) { console.log( r ); } );
```

If the optional argument `fail-or-option` is a procedure. It is invoked
if an error occurs while invoking the service. If `fail-or-option` is
an object. Here are the attributes this object may contain:

 * `host`, the host name on which the service is invoked. This option is
only used when invoking service from servers to servers.
 * `port`, the port number of the remote host. This option is
only used when invoking service from servers to servers.
 * `user`, a user identity on behalf of who the service is invoked.
 * `password`, the user password.
 * `fail`, a failure procedure.
 * `asynchronous`, a boolean.
 * `scheme`, the schema to be used to invoke the service. Should normally be
either `http` or `https`.
 * `ssl`, a boolean which specifies if SSL is to be used to invoke the service.
 * `header`, a JavaScript object describing the HTTP header of the request.

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

### frame.post( [ success, [ fail-or-option ] ] ) ###
[:@glyphicon glyphicon-tag function]

The asynchronous version of `postSync`.

### HopFrame as URLs ###

HopFrame can be everywhere a URL is expected, in particular, in HTML
nodes. For instance, the `src` attribute of an image can be filled with
an HopFrame. In that case, the content of the image, we be the result
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

Invoking the `post` or `postSync` methods of a service frame raised
the remote invocation of the service. That is, the arguments serialized
in the service frame are transmitted to the remote host and the service
body is executed.

${ <span class="label label-warning">Note:</span> }
The `this` implicit argument of the service invocation is a request
object. This object contains all the information about the current HTTP
request.
[:@warning]

Example:

```hopscript
service svc( { name: "durand" } ) {
  console.log( "host=", this.host, " port=", this.port );
  console.log( "abspath=", this.abspath );
  return true;
}
```

Service are free to return any _serializable_ object. The value
is first converted into a `hop.HTTPResponse` object by Hop. This converted
value is sent to the client. The rules for converting values into
`hop.HTTPResponse` are as follows:

 * if the response is a string, Hop constructs a `hop.HTTPResponseString`
 object.
 * if the response is a XML document, a `hop.HTTPResponseXML` object is
 construct.
 * If the response is a promise, a `hop.HTTPResponseAsync` object is built.
 * if the response is a JavaScript object.
   * if that object has a `toResponse` property which is a function, the
   result of invoking this function is used as a reponse.
 * Otherwise, a `hop.HTTPResponseHop` is construct. This will have the
 effect of serializing the JavaScript object and re-creating it on the client.

The various Hop responses classes are documented [here](00-hop.html#responses).

Service methods & attributes
----------------------------

### service.path ###
[:@glyphicon glyphicon-tag parameter]

The path (_i.e.,_ the absolute path of the URL) of the associated service.
This is a string must be prefixed by `/hop/`.

Example

```hopscript
svc2.path = "/hop/dummy";
```

### service.resource( file ) ###
[:@glyphicon glyphicon-tag function]

Create the absolute relatively to the file defining the service. For instance,
this can be used to obtained the absolute path of CSS file or an image whose
name is known relatively to the source file defining the service.

Example:

${ doc.include( doc.EXAMPLES_DIR + "/svc/README.md" ) }

${ <span class="label label-info">svc/svc.js</span> }

```hopscript
${ doc.include( doc.EXAMPLES_DIR + "/svc/svc.js", 13 ) }
```

### service.timeout ###
[:@glyphicon glyphicon-tag parameter]

The number of seconds the service is live. Negative values means infinite
timeout.

Example

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
