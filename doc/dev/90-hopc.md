${var hop = require( "hop" )}
${var doc = require( "hopdoc" )}


Hopc Development
================

${ <span class="label label-warning">Note:</span> } 
This documentation page is intended for export developers only, in particular
those that are willing to experiment with new compilation optimizations or
analysis.
[:@warning]


Hopc is the Hop multi-language compiler. It accepts has source languages
[Scheme](http://www-sop.inria.fr/indes/fp/Bigloo/) and 
[JavaScript](https://www.ecma-international.org/ecma-262/) and it can generates
any of these two languages as target. This section describes how to modify the
compiler when the source language is JavaScript. 

The Hopc JavaScript compiler is multi-staged. As of branch 3.3 it
contains about 50 stages. The compiler can be tweaked in two ways:

  * modifying the order of stage executions.
  * using new custom compilation stages.


Multi-staged compilation
------------------------

In the compiler terminology, a _driver_ is the compiler entity that controls
how stages flow one from the other. The compiler exports a list of
builtin drivers. This list can be obtained with:

```shell
% hopc --js-drivers-list
```

A particular builtin driver can be selected with:

```shell
% hopc --js-driver DRIVER-NAME
```

The list and order of compilation stages that correspond to that driver
are obtained with:

```shell
% hopc --js-driver DRIVER-NAME --js-show-driver
```

Custom drivers can be used to compile a source file by explicitly listing
the stages to use. Example:

```shell
% hopc --js-driver syntax,hopscript-header,loopexit,bestpractice,symbol,this,read-only,return,property,scheme foo.js -v2
```

Note that when the option `-v2` or higher is used on the command line, the
compiler displays all the compilation stages it executes.

If the shell `HOPTRACE` environment variable is bound to a string
prefixed with `j2s:`, for instance `HOPTRACE=j2s:stage`, the compiler will
dump its internal abstract syntax tree after executing each stage. These
dumps will be located in `/tmp/$USER/J2S/your-source/...`.


External Compilation Stages
---------------------------

External compilation stages can be used to specify a custom compilation
driver. For that the URL of the stage is used instead of a the name of
a builtin stage. For instance, let us assume that a Hop server is running
and accepting connections on port `8888` and let us assume that it implements
a compilation stage accessible with a service named `js2http`, then
a source compilation can be obtained with:

```shell
% hopc --js-driver syntax,hopscript-header,loopexit,bestpractice,symbol,this,read-only,return,property,http://localhost:8888/hop/js2http,scheme foo.js -v2
```

The service invoked by the compiler will receive one object with two 
properties:

  * `ast`: the compiler AST.
  * `config`: the compilation configuration.
  
The service will have to return a valid AST that will be used by the following
compilation stages.

Several external compilation stages can be used in one compilation.

  
The Abstract Syntax Tree
------------------------

The Hopc Abstract Syntax Tree (AST henceforth) is defined as a Scheme
class hierarchy declared in the file `hop/js2scheme/ast.scm`. The Hop
`hop.hopc` module enables easy access and manipulation from within
JavaScript. This modules maps all the Scheme class to JavaScript
classes and it provides two functions used to import and export the
AST.

### hopc.intern( ast ) ###
[:@glyphicon glyphicon-tag function]

This function accepts as input an AST as received by the service
implementing the stage and it re-constructs the sharing between AST
nodes that have been serialized by the compiler. This is named an
_interned_ ast.

A compilation *must* return an interned ast, so the simplest possible
compilation stage is:

```hopscript
const hopc = require( hop.hopc );

service js2http( { ast, config } ) {
    return hopc.intern( ast );
}	
```

A compilation stage can modify or annotate the ast it receives. These 
modifications will be visible to the following compilation stages.


### hopc.extern( ast ) ###
[:@glyphicon glyphicon-tag function]

Interned ast cannot be serialized into the JSON format using the
regular `toJSON` function as these trees are cyclic data structure.
The function `hopc.extern` serializes the tree into a JSON
representation handling the sharing so that they could be re-established
by the `hopc.intern` function.

Here is an example of a compilation stage that dumps its tree in
a json file and read it again for its return value.


${ <span class="label label-info">js2json.js</span> }

```hopscript
${ doc.include( doc.BUILDDIR + "/doc/dev/js2json.js" ) }
```

Ast Walker
----------

The `hop.hopc` module provides a facility for traversing ast.

### new hopc.HiopcAstWalker() ###
[:@glyphicon glyphicon-tag constructor]

Builds an ast walker. 

### walker.walk( prg ) ###
[:@glyphicon glyphicon-tag function]

Walks along the ast using the depth-first traversal. That is, this function
traverses all the ast node scanning all nodes fields. 

The traversal of each node can be individually modified by declaring
new methods to a walker. Methods are named after class names. For
instance, the following declares a custom walker for the nodes that
correspond to JavaScript object accesses:

```hopscript
const w = new hopc.HopcAstWalker();
w.J2SAccess = function( node ) {
   console.log( "an access ", node.loc );
   return node;
}
w.walk( prg );
```

A Complete Example
------------------

${ doc.include( doc.BUILDDIR + "/examples/js2http/README.md" ) }

${ <span class="label label-info">js2http/js2http.js</span> }

```hopscript
${ doc.include( doc.BUILDDIR + "/examples/js2http/js2http.js", 17 ) }
```


