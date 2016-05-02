${ var doc = require( "hopdoc" ) }

Web Reactive Programming
========================

Hop.js Web Reactive Programming (WRP) enables DOM nodes components to
be automatically updated or modified to reflect modifications of the
program manipulated data. The components might be nodes attributes,
nodes themselves that be can be modified or replaced, or even new
nodes that can be added.

WRP ressorts on two components: the _sources_ and the _sinks_.  The
sources are surpervised data structures whose modification yield to
DOM updates. The _sinks_ are the DOM components that are updated
when sinks changes. They might be DOM nodes attributes and DOM nodes
themselves.

Here is a first example, inspired by a REACT.js example that creates
a web page showing the chronometers. The _source_ is the `state` variable.
The _sink_ is the `react` node. The modification of the `state` variables
that occur each seconds automatically yield to updating the web
page.

${ <span class="label label-info">reactsrv/stateful.js</span> }

```hopscript
${ doc.include( doc.BUILDDIR + "/doc/lang/stateful.js" ) }
```

A second example, also inspired by the READ.js web site:


${ <span class="label label-info">reactsrv/app.js</span> }

```hopscript
${ doc.include( doc.BUILDDIR + "/doc/lang/app.js" ) }
```

Methods
-------

Sources are implemented a subclass of the JavaScript proxy objects.
As such only allocated objects can be used as sources, by opposition
to numbers, booleans, `undefined`, `null`, or literal strings.


### hop.reactProxy( object ) ###
[:@glyphicon glyphicon-tag function]

The `hop.reactProxy` method creates a reactive source out of 
`object`. Each modification to `object` will yield to updating the
DOM nodes that depends on that object.

### server.reactProxy( event, value ) ###
[:@glyphicon glyphicon-tag function]

The `server.reactproxy` method creates a reactive source out of
a server event. Each time `event` is receive, the DOM is updated
accordingly. The default `value` is used until `event` is received.

${ <span class="label label-info">reactsrv/reactsrv.js</span> }

```hopscript
${ doc.include( doc.BUILDDIR + "/examples/reactsrv/reactsrv.js", 14 ) }
```

${ doc.include( doc.BUILDDIR + "/examples/reactsrv/README.md" ) }


HTML tag
--------

### <REACT> ###
[:@glyphicon glyphicon-tag tag]

The body of a `<react>` node is client script that gets re-evaluated
each time of its source is modified. The result of that evaluated in
inserted in the DOM tree, possibly replacing already created dynamic
nodes.

${ <span class="label label-info">react/react.js</span> }

```hopscript
${ doc.include( doc.BUILDDIR + "/examples/react/react.js", 14 ) }
```

${ doc.include( doc.BUILDDIR + "/examples/react/README.md" ) }



