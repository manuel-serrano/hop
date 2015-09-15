${ var doc = require( "hopdoc" ) }

Dom
===

Hop supports the DOM interface on both client and server. The
client-side DOM is complete. The server-side DOM is under development
and slightly differs from the client-side DOM as the server does not
implement a _unique_ document. As a consequence, an XML element can
be simultaneously contained in various XML nodes. On the server,
Inserting and a node in a document does not not automatically removes
it from another parent where it could been included initially, then
the `nextSibling`, `previousSibling`, and `parentNode` properties
cannot be used with non arborescent structures.

Examples
--------

${ doc.include( doc.EXAMPLES_DIR + "/dom/README.md" ) }

${ <span class="label label-info">dom/dom.js</span> }

```hopscript
${ doc.include( doc.EXAMPLES_DIR + "/dom/dom.js", 14 ) }
```


Node attributes
---------------

### node.id ###
[:@glyphicon glyphicon-tag parameter]

The `id` of the node.

### node.nodeType ###
[:@glyphicon glyphicon-tag parameter]

Return an integer value which specifies the type of the node; possible
values are listed in [Node type constants](https://developer.mozilla.org/en/docs/Web/API/Node/nodeType).

### node.childNodes ###
[:@glyphicon glyphicon-tag parameter]

The children of the node in an array.

### node.className ###
[:@glyphicon glyphicon-tag parameter]

The class of the node.

### node.innerHTML ###
[:@glyphicon glyphicon-tag parameter]

Set or get the HTML syntax describing the element's descendants.
 
### node.nextSibling ###
[:@glyphicon glyphicon-tag parameter]

The next sibling of the node.

### node.outerHTML ###
[:@glyphicon glyphicon-tag parameter]

The serialized HTML fragment describing the element including its
descendants. It can be set to replace the element with nodes parsed
from the given string.

### node.parentNode ###
[:@glyphicon glyphicon-tag parameter]

The parent of the node or undefined if the node is not
contained in a document.

### node.previousSibling ###
[:@glyphicon glyphicon-tag parameter]

The previous sibling of the node.

### node.tagName ###
[:@glyphicon glyphicon-tag parameter]

The tag name of the node.

Node methods
------------

### node.appendChild( child ) ###
[:@glyphicon glyphicon-tag function]

Add a new child to `node`.

### node.removeChild( child ) ###
[:@glyphicon glyphicon-tag function]

Remove a new child to `node`.

### node.getElementById( id ) ###
[:@glyphicon glyphicon-tag function]

Return the child of node whose id is `id`.

### node.getElementsByTagName( name ) ###
[:@glyphicon glyphicon-tag function]

Return the children of node whose tag matches `name`.

${ <span class="label label-warning">Note:</span> }
 On the server, this method is supported by all nodes. Contrary to the client
that only supports it for the `document` object. 
[:@warning]

### node.getElementsByClassName( name ) ###
[:@glyphicon glyphicon-tag function]

Return the children of node whose class contains `name`.

${ <span class="label label-warning">Note:</span> }
 On the server, this method is supported by all nodes. Contrary to the client
that only supports it for the `document` object. 
[:@warning]

