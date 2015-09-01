${ var doc = require( "hopdoc" ) }

WebSockets
==========

Hop supports server WebSockets and client WebSockets.

${ doc.include( doc.EXAMPLES_DIR + "/websocket/README.md" ) }

${ <span class="label label-info">websocket/wsserver.js</span> }

```hopscript
${ doc.include( doc.EXAMPLES_DIR + "/websocket/wsserver.js", 13 ) }
```

${ <span class="label label-info">websocket/wsclient.js</span> }

```hopscript
${ doc.include( doc.EXAMPLES_DIR + "/websocket/wsclient.js", 13 ) }
```

Constructors
------------

### new WebSocketServer( url [, option ] ) ###
[:@glyphicon glyphicon-tag function]

The argument `option` is either:

 * a string, in which case, it specifies the path of the WebSocketServer.
 * an object, in which case the following properties are read:
   * `path`: the WebSocketServer path;
   * `protocol`: the protcolo.


### new WebSocket( host [, option ] ) ###
[:@glyphicon glyphicon-tag function]

The argument `option` is either:

 * a string, in which case, it specifies the _protocol_ of the WebSocket.
 * a array of strings, which are the procols of the WebSocket.
 * an object, in which case, the field `protocol` is used as a raw
 string describing the WebSocket protocol.


Properties
----------

### WebSocketServer.onconnection ###
[:@glyphicon glyphicon-tag parameter]

An `EventListener` called whenever a WebSocketServer establishes a new
connection with a remote WebSocket client.

### WebSocket.readyState ###
[:@glyphicon glyphicon-tag parameter]

The state of the WebSocket.

### WebSocket.url ###
[:@glyphicon glyphicon-tag parameter]

The URL of the WebSocket.

### WebSocket.onmessage ###
[:@glyphicon glyphicon-tag parameter]

An `EventListener` called whenever the WebSocket receives a message.

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

### WebSocket.send( msg ) ###
[:@glyphicon glyphicon-tag function]

Send `msg`, a string, to the WebSocket.

### WebSocket.close() ###
[:@glyphicon glyphicon-tag function]

Close a WebSocket.

