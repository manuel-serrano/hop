${ var doc = require( "hopdoc" ) }
${ var fontifier = require( "fontifier" ) }

Server-side Workers
===================

Hop server-side workers are processes that run concurrently. Each
worker possesses its own isolated memory heap. Workers cannot share
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
the source file of the worker. The path is resolved using the same rules
as the `require` module importation function
(see [module](./01-module.html)).

Example:

```hopscript
var w = new Worker( "./slave.js" );
```

`New Worker( path )` returns immediately, before the worker module is
fully initialized. Messages sent to the worker are buffered until the
worker is able to process them, so the calling thread may send
messages as soon as the `Worker`object is created.

However, since workers may also define services, the developer should
ensure that services defined within the worker will not be invoked
until the worker is fully initialized.  In general, this is achieved
by letting the worker pass the service handle to the calling thread.
Note that services that are invoked as a result of a user action on a
web client are very likely to be up and running long before the user
clicks on the button triggering the service invocation.

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

An `EventListener` called whenever the worker receives a message. The
message is stored in the `data`property of the event.

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
 4. `Service` handles can be passed within a message.
 5. Objects made from the above data types are passed with the same
    donation rules.

6. Objects that cannot be donated (`Worker`, `Arguments`, `Function`, `Math`,
`Json`, `Error`, or `Promise`) are replaced with the `undefined` value.

### Basic communications ###

${ doc.include( doc.ROOT + "/examples/worker/README.md" ) }

${ <span class="label label-info">worker/worker.js</span> }

```hopscript
${ doc.include( doc.ROOT + "/examples/worker/worker.js", 14 ) }
```

${ <span class="label label-info">worker/slave.js</span> }

```hopscript
${ doc.include( doc.ROOT + "/examples/worker/slave.js", 14 ) }
```

${ <span class="label label-info">worker/bar.js</span> }

```hopscript
${ doc.include( doc.ROOT + "/examples/worker/bar.js", 14 ) }
```

### Worker.terminate() ###
[:@glyphicon glyphicon-tag function]

Terminates the worker as soon as possible.

${ doc.include( doc.EXAMPLES_DIR + "/worker2/README.md" ) }

${ <span class="label label-info">worker2/worker2.js</span> }

```hopscript
${ doc.include( doc.EXAMPLES_DIR + "/worker2/worker2.js", 14 ) }
```

${ <span class="label label-info">worker2/slave.js</span> }

```hopscript
${ doc.include( doc.EXAMPLES_DIR + "/worker2/slave.js", 14 ) }
```


Workers and Services
--------------------

${ doc.include( doc.ROOT + "/examples/worker5/README.md" ) }

${ <span class="label label-info">worker5/worker5.js</span> }

```hopscript
${ doc.include( doc.ROOT + "/examples/worker5/worker5.js", 14 ) }
```

${ <span class="label label-info">worker5/slave.js</span> }

```hopscript
${ doc.include( doc.ROOT + "/examples/worker5/slave.js", 14 ) }
```

