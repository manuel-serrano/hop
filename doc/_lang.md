Compatibilities & Standards
===========================
[:@nonumber]

Hop.js is a multi-language platform. It can either be programmed
in HopScript, which is a super-set of [JavaScript][es5], or in
a variant of the [Scheme][r5rs] programming. This documentation only
presents the HopScript language and environment. Programming Hop.js
in Scheme is decribed [here](http://hop.inria.fr/hop/doc).

HopScript is based on JavaScript and aims at ensuring compatibility
with this language.

  * HopScript fully supports [ECMAScript 5][es5]
  * HopScript also support some [ECMAScript 6][es6] features:
    * arrow functions.
    * default function parameters.
    * rest arguments.
    * `let` and `const` bindings.
    * symbols.
    * template strings.
    * promises.
    * typed Arrays.
  * HopScript supports most [Nodejs][nodejs] APIs (see Section
  [Nodejs](nodejs.html))
    
${ <span class="label label-warning">Note:</span> }
 ECMAScript 6 features requires `use strict` or `use hopscript` modes.
[:@warning]

HopScript mode
==============

The `use hopscript` mode is an even stricter mode that `use strict` mode.
Its caracteristics are:

  * Function declarations are immutables.
  * Arity checks are enforced and errors are raised on mistmatch.

[es5]: http://www.ecma-international.org/ecma-262/5.1
[es6]: http://www.ecma-international.org/ecma-262/6.0
[nodejs]: https://nodejs.org/api
[r5rs]: http://www-sop.inria.fr/indes/fp/Bigloo/doc/r5rs.html
