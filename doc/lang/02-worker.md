${ var doc = require( "hopdoc" ) }
${ var fontifier = require( "fontifier" ) }

Server-side Workers
===================

Hop server-side workers are processes that run concurently. Each
worker possses its own isolated memory heap. Workers cannot shared
JavaScript objects. Each worker possesses its own private JavaScript
global objects. Worker communicate using message passing. Workers
are the only mean for running concurrent JavaScript execution on the
server.

${ <span class="label label-warning">Note:</span> }
This page only documents server-side workers. The documentation of
client-side workers can be found [here](https://developer.mozilla.org/en/docs/Web/API/Worker).
[:@warning]

Constructor
-----------

### new Worker( path ) ###
[:@glyphicon glyphicon-tag function]

Workers are created using the `Worker` function which takes as parameter
the source file of the worker. The path is resolved using the rules
as the `require` module importation function
(see [nodejs module](https://nodejs.org/api/modules.html)).

Example:

```hopscript
var w = new Worker( "./slave.js" );
```

Properties
----------

### Worker.onerror ###
[:@glyphicon glyphicon-tag parameter]

An `EventListener` called whenever an error occurs on the worker.

### Worker.onexit ###
[:@glyphicon glyphicon-tag parameter]

An `EventListener` called whenever the worker exits.

### Worker.onmessage ###
[:@glyphicon glyphicon-tag parameter]

An `EventListener` called whenever the worker receives a message.

Methods
-------

### Worker.postMessage( msg ) ###
[:@glyphicon glyphicon-tag function]

Send a message to a worker. The argument `msg` is a JavaScript which
is _donated_ to the target. Donation depends on the type of the
objects. The following rules apply:

 1. If `msg` is an immediate value, the value is transmitted as is.
 2. If `msg` is an `Array`, an `ArrayBuffer`, an `ArrayBufferView`, or a
 `TypedBuffer`, the array is donated to the target worker. All the values are
 donated to the target worker. The source worker is left with an array of
 length 0.
 3. In all our cases, `msg` is deep copied an each field is donated to the
 target.

Objects that cannot be donated (`Worker`, `Arguments`, `Function`, `Math`,
`Json, `Error`, or `Promise`) are replaced with the `undefined` value.

### Basic communications ###

${ doc.include( doc.ROOT + "/examples/worker/README.md" ) }

${ <span class="label label-info">worker/worker.js</span> }

```hopscript
${ doc.include( doc.ROOT + "/examples/worker/worker.js", 13 ) }
```

${ <span class="label label-info">worker/slave.js</span> }

```hopscript
${ doc.include( doc.ROOT + "/examples/worker/slave.js", 13 ) }
```

${ <span class="label label-info">worker/bar.js</span> }

```hopscript
${ doc.include( doc.ROOT + "/examples/worker/bar.js", 13 ) }
```

### Worker.terminate() ###
[:@glyphicon glyphicon-tag function]

Terminates the worker as soon as possible.

${ doc.include( doc.EXAMPLES_DIR + "/worker2/README.md" ) }

${ <span class="label label-info">worker2/worker2.js</span> }

```hopscript
${ doc.include( doc.EXAMPLES_DIR + "/worker2/worker2.js", 13 ) }
```

${ <span class="label label-info">worker2/slave.js</span> }

```hopscript
${ doc.include( doc.EXAMPLES_DIR + "/worker2/slave.js", 13 ) }
```


Workers and Services
--------------------

${ doc.include( doc.ROOT + "/examples/worker5/README.md" ) }

${ <span class="label label-info">worker5/worker5.js</span> }

```hopscript
${ doc.include( doc.ROOT + "/examples/worker5/worker5.js", 13 ) }
```

${ <span class="label label-info">worker5/slave.js</span> }

```hopscript
${ doc.include( doc.ROOT + "/examples/worker5/slave.js", 13 ) }
```

