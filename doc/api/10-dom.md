${ var doc = require( "hopdoc" ) }

Dom
===

Hop supports the DOM interface on both client and server. The
client-side DOM is complete. The server-side DOM is under development
and slightly differs from the client-side DOM as the server does not
implement a __unique__ document. As a consequence, an XML element can
be simultaneously contained in various XML nodes. On the server,
Inserting and a node in a document does not not automatically removes
it from another parent where it could been included initially.

Examples
--------

${ doc.include( doc.EXAMPLES_DIR + "/dom/doc.md" ) }

${ <span class="label label-info">dom/dom.js</span> }

```hopscript
${ doc.include( doc.EXAMPLES_DIR + "/dom/dom.js", 13 ) }
```


Node attributes
---------------

### node.id ###

The `id` of the node.

### node.childNodes ###

The children of the node.

### node.className ###

The class of the node.

### node.nextSibling ###

The next sibling of the node.

### node.parentNode ###

The parent of the node or undefined if the node is not
contained in a document.

### node.previousSibling ###

The previous sibling of the node.

### node.tagName ###

The tag name of the node.

Node methods
------------

### node.appendChild( child ) ###

Add a new child to `node`.

### node.removeChild( child ) ###

Remove a new child to `node`.

### node.getElementsByTagName( name ) ###

Return the children of node whose tag match `name`.

${ <span class="label label-warning">Note:</span> }
 On the server, this method is supported by all nodes. Contrary to the client
that only supports it for the `document` object. 
[:@warning]

### node.getElementsByClassName( name ) ###

Return the children of node whose tag match `name`.

${ <span class="label label-warning">Note:</span> }
 On the server, this method is supported by all nodes. Contrary to the client
that only supports it for the `document` object. 
[:@warning]

