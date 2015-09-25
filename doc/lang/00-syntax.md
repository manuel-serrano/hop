${ var doc = require( "hopdoc" ) }
${ var path = require( "path" ) }
${ var ROOT = path.dirname( module.filename ) }

HopScript Syntax
================

HopScript extends EcmaScript 5 syntax with the following additional constructs:

 * service definitions and declarations;
 * `\~`-expressions and `\$`-expressions;
 * embedded HTML syntax.

The formal syntax is defined by:


Services
--------

Services share most of their syntax with functions, with two notable differences:

 * services are introduced with the `service` keyword;
 * services arguments may be named using the JavaScript object literal
   syntax.

```ebnf
${ doc.include( ROOT + "/service.bnf" ) }
```

### Example ###

${ <span class="label label-info">syntax/syntax.js</span> }

```hopscript
${ doc.include( ROOT + "/../../examples/syntax/syntax.js", 14, 72 ) }
```
 
Tilde
-----

HopScript client-side programs are values created by server-side computations.
They have a dedicated syntax: the `\~`-expression. Server-side
values can be injected a `\~`-expression using the `\$`-expressions.

```ebnf
${ doc.include( ROOT + "/tilde.bnf" ) }
```

HTML
----

HopScript supports two syntaxes for creating server side HTML values. The first
one is plain HTML syntax extended with `\~`- and `\$`-expressions. The second one
is an extension of JavaScript literal objects.

```ebnf
${ doc.include( ROOT + "/html.bnf" ) }
```

### Example ###

${ <span class="label label-info">syntax/syntax.js</span> }

```hopscript 
${ doc.include( ROOT + "/../../examples/syntax/syntax.js", 72 ) }
```



