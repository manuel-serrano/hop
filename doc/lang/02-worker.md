${ var doc = require( "hopdoc" ) }
${ var fontifier = require( "fontifier" ) }

Server-side Workers
===================

Hop server-side workers are processes that run concurently on the server.
Each worker possses its own isolated memory heap. That is, workers cannot
shared JavaScript objects. They have different JavaScript global objects.

### new hop.Worker( path ) ###
[:@glyphicon glyphicon-star]


Examples
--------

### svc.js ###

${ doc.include( doc.ROOT + "/examples/worker/doc.md" ) }

${ <span class="label label-info">worker.js</span> }

```hopscript
${ doc.include( doc.ROOT + "/examples/worker/worker.js", 13 ) }
```

${ <span class="label label-info">slave.js</span> }

```hopscript
${ doc.include( doc.ROOT + "/examples/worker/slave.js", 13 ) }
```

${ <span class="label label-info">bar.js</span> }

```hopscript
${ doc.include( doc.ROOT + "/examples/worker/bar.js", 13 ) }
```

