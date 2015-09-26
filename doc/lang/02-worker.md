${ var doc = require( "hopdoc" ) }

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

Terminology
-----------

We define the parent thread as the thread which executes the `new Worker`
constructor, and the worker thread as the thread which is created by
the system in response to the `new Worker` instruction.

A worker thread is attached to one and only one  parent thread.

Any thread may be the parent thread of multiple worker threads.

The parent to worker threads connection graph is a tree, rooted by the
main thread of process.

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
worker is able to process them, so the parent thread may send
messages as soon as the `Worker`object is created.

However, since workers may also define services, the developer should
ensure that services defined within the worker will not be invoked
until the worker is fully initialized.  In general, this is achieved
by letting the worker pass the service handle to the parent thread.
Note that services that are invoked as a result of a user action on a
web client are very likely to be up and running long before the user
clicks on the button triggering the service invocation.

Properties
----------

The following properties define `EventListeners` for the parent
thread, attached to `Worker`.

### Worker.onerror ###
[:@glyphicon glyphicon-tag parameter]

An `EventListener` called whenever an error occurs on `Worker`.

### Worker.onexit ###
[:@glyphicon glyphicon-tag parameter]

An `EventListener` called when `Worker` terminates.

### Worker.onmessage ###
[:@glyphicon glyphicon-tag parameter]

An `EventListener` called whenever the parent thread receives a
message from the `Worker` thread. The message is stored in the
`data`property of the event.


Variables
-----------
 
The following variable can be set in a worker thread, it is
implicitely attached to the parent thread.

### onmessage ###
[:@glyphicon glyphicon-tag parameter]

An `EventListener` called whenever the worker thread receives a
message from its parent thread. The message is stored in the
`data`property of the event.


Methods
-------

### Worker.postMessage( msg ) ###
[:@glyphicon glyphicon-tag function]

Use this Method in the parent thread to send a message to a
worker. The argument `msg` is a JavaScript object which is _donated_
to the target. Donation depends on the type of the objects. The
following rules apply:

 1. If `msg` is an immediate value, the value is transmitted as is.
 2. If `msg` is an `Array`, an `ArrayBuffer`, an `ArrayBufferView`, or
 a `TypedBuffer`, the array is donated to the target worker. All the
 values are donated to the target worker. The source worker is left
 with an array of length 0.
 3. In all other cases, `msg` is deep copied and each field is donated
 to the target.
 4. `Service` handles can be passed within a message.
 5. Objects made from the above data types are passed with the same
    donation rules.
 6. Objects that cannot be donated (`Worker`, `Arguments`, `Function`,
`Math`, `Json`, `Error`, or `Promise`) are silently replaced with the
`undefined` value.

### postMessage( msg ) ###
[:@glyphicon glyphicon-tag function]

Use this function in the Worker thread to send a message to the
implicit parent.

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

${ <span class="label label-info">worker/bar.js</span> } Note that the
parent thread and the Worker thread each get their instance of
`bar.js`.

```hopscript
${ doc.include( doc.ROOT + "/examples/worker/bar.js", 14 ) }
```

### Worker.terminate() ###
[:@glyphicon glyphicon-tag function]

Terminates `Worker` as soon as possible. Sub Workers are recursively
terminated.

### terminate() ###
[:@glyphicon glyphicon-tag function]

Use the function `terminate` to self terminate a Worker. Sub Workers
are recursively terminated.

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

