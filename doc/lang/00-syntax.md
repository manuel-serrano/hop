${ var doc = require( "hopdoc" ) }
${ var fontifier = require( "fontifier" ) }

HopScript Syntax
================

HopScript extends EcmaScript 5 syntax with the following additional constructs:

 * service definitions and declarations;
 * `~`-expessions and `$`-expressions;
 * embedded HTML syntax.

The formal syntax is defined by:


Services
--------

Services shares most of their syntax with functions, with two notable differences:

 * services are introduced with the `service` keyword;
 * services supported named arguments which are introduced using the JavaScript
   object literal syntax. 

```ebnf
${ doc.include( ROOT + "/service.bnf" ) }
```

### Example ###

```hopscript
${ doc.include( ROOT + "/../../examples/tower/tower.js", 13 ) }
```

Tilde
-----

HopScript client-side programs are values created by server-side computations.
They have a dedicated syntax: the `~`-expression. Server-side
values can be injected a `~`-expression using the `$`-expressions.

```ebnf
${ doc.include( ROOT + "/tilde.bnf" ) }
```

HTML
----

HopScript supports two syntaxes for creating server side HTML values. The first
one is plain HTML syntax extended with `~`- and `$`-expessions. The second one
is an extension of JavaScript literal objects.

```ebnf
${ doc.include( ROOT + "/html.bnf" ) }
```



