Compatibilities & Standards
===========================
[:@nonumber]

HopScript is based on JavaScript and aim at ensuring compatibility
with this language.

  * HopScript fully supports [ECMAScript 5][es5]
  * HopScript also support some [ECMAScript 6][es6] features:
    * arrow functions.
    * default function parameters.
    * `let` and `const` bindings.
    * symbols.
    * template strings.
    * promises.
    * Typed Arrays.
  * HopScript supports most [Nodejs][nodejs] APIs (see Section
  [Nodejs](nodejs.html))
    
${ <span class="label label-warning">Note:</span> }
 ECMAScript 6 features requires `use strict` or `use hopscript` modes.
[:@warning]

```hopscript  
service() { return foo }
```

[es5]: http://www.ecma-international.org/ecma-262/5.1
[es6]: http://www.ecma-international.org/ecma-262/6.0
[nodejs]: https://nodejs.org/api
