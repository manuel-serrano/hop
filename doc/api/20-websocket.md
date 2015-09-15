${ var doc = require( "hopdoc" ) }

WebSockets
==========

Hop supports server WebSockets and client WebSockets.

Server WebSockets are system objects that listen to the Hop built-in
multi-threaded http server. Connection parameters are thus the same as
for the http server.

Client WebSockets enable a Hop process to connect to a Hop WebSocket
server or to a third party WebSocket server.

Both server and client WebSocket API are built-in in Hop.js, no need
to require a module.

WebSockets clients may also be created in Hop browser code (JavaScript
scripts running in a web browser). In such a case, refer to the
standard WebSocket API for web browsers.


${ doc.include( doc.EXAMPLES_DIR + "/websocket/README.md" ) }

${ <span class="label label-info">websocket/wsserver.js</span> }

```hopscript
${ doc.include( doc.EXAMPLES_DIR + "/websocket/wsserver.js", 14 ) }
```

${ <span class="label label-info">websocket/wsclient.js</span> }

```hopscript
${ doc.include( doc.EXAMPLES_DIR + "/websocket/wsclient.js", 14 ) }
```

Constructors
------------

### new WebSocketServer( url [, option ] ) ###
[:@glyphicon glyphicon-tag function]

The argument `option` is either:

 * a string, in which case, it specifies the path of the
   WebSocketServer. Note that `/hop/` is prepended to the url argument.
 * an object, in which case the following properties are read:
   * `path`: the WebSocketServer path;
   * `protocol`: the protocol.


### new WebSocket( uri [, option ] ) ###
[:@glyphicon glyphicon-tag function]

The argument `uri` is a complete URI, prefixed with either `ws:`or
`wss:` depending on whether http or https is to be used. Contrary to
the server side, the `/hop/`prefix must be explicitely provided in the
path to connect to a Hop WebSocket server (but clients may also
connect to third party servers).

The argument `option` is either:

 * a string, in which case, it specifies the _protocol_ of the WebSocket.
 * a array of strings, which are the protocols supported by the client.
 * an object, in which case, the field `protocol` is used as a raw
 string describing the WebSocket protocol.


Properties
----------

### WebSocketServer.onconnection ###
[:@glyphicon glyphicon-tag parameter]

An `EventListener` called whenever a WebSocketServer establishes a new
connection with a remote WebSocket client. The newly created WebSocket
is in the `value`property of the event argument.

### WebSocketServer.onclose ###
[:@glyphicon glyphicon-tag parameter]

An `EventListener` on the `close`event called when a WebSocketServer is closed (see
method `WebSocketServer.close`).

### WebSocket.readyState ###
[:@glyphicon glyphicon-tag parameter]

The state of the WebSocket.

### WebSocket.url ###
[:@glyphicon glyphicon-tag parameter]

The URL of the WebSocket.

### WebSocket.onmessage ###
[:@glyphicon glyphicon-tag parameter]

An `EventListener` called whenever the WebSocket receives a
message. The message payload is in the `data` property of the event.

### WebSocket.onopen ###
[:@glyphicon glyphicon-tag parameter]

An `EventListener` called whenever the WebSocket is open.

### WebSocket.onclose ###
[:@glyphicon glyphicon-tag parameter]

An `EventListener` called whenever the WebSocket is closed.

### WebSocket.onerror ###
[:@glyphicon glyphicon-tag parameter]

An `EventListener` called whenever an error occurs on the WebSocket.

Methods
-------

### WebSocketServer.close() ###
[:@glyphicon glyphicon-tag function]

Stops the WebSocketServer service.

### WebSocket.send( msg ) ###
[:@glyphicon glyphicon-tag function]

Send `msg`, a string, to the WebSocket.

### WebSocket.close() ###
[:@glyphicon glyphicon-tag function]

Close a WebSocket.

