${ var doc = require( "hopdoc" ) }
${ var path = require( "path" ) }
${ var ROOT = path.dirname( module.filename ) }

JS2Scheme type inference
========================

Types relationships
-------------------

  * `unknown`: uncomputed value type
  * `any`: universal type (biggest type)
  * `scm`: a Scheme value
  * `cmap`: a hidden class map (a Scheme value)
  * `void`: the pseudo statements type 
  * `object`: a JS `Object` value
  * `global`: the JS `global object` value
  * `number`: a number
  * `integer` < `number`: an integer (a JS integer)
  * `index` < `integer`: a JS index [0..2^32-1[
  * `u8` < `integer`: a JS `uint8`
  * `string`: a JS string
  * `scmstring`: a native string
  * `regex`: a JS regular expression
  * `undefined`: JS `undefined` value
  * `null`: JS `null` value
  * `bool`: JS boolean
  * `date`: JS date
  * `array`: a JS array
  * `u8array`: a JS array
  * `tilde`: a JS client side expression


AST types
---------

Types are represented as symbol in the AST. They are included in the
following AST nodes:

  * `J2SExpr.type` [rw]: the type of that expression. Default to `unknown`.
  * `J2SDecl.utype` [ro]: the user type annotation for that declaration.
  No guaranty applies to that type. 
  * `J2SDecl.itype` [rw]: the initial type of the declaration. It is sound.
  When used in a function declaration, it means that this function is
  _always_ called with that type.
  * `J2SFun.rtype` [rw]: the return type of a function.
  

Methodology
-----------

There are the main principles that govern the typing of the AST.

  1. Types are held by references
  2. A variable may have different types according to its different uses
  3. A decl holds an itype (let it be a local variable or a function parameter)
  iff:
    a. it's initializing value is of the desired type (for functions it means
    that the function is always called with the proper type).
	b. it holds that type time during all its lifetime and for all its
	occurrences.


Rules
-----

### fun ###

Type `fun.body` in an environment containing only the typed parameters.

### try ###

`try.body`, `try.catch`, and `try.finally` are treated as independant.
The typing of the `try` form does not add any information to the current
environment (as it is statically unknown how much of the `body` is executed).


Hint typing
-----------

Hints are held by variable declarations. They are represented by list
of type-name, occurrence pairs.
