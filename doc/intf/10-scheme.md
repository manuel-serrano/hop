${ var doc = require( "hopdoc" ) }
${ var path = require( "path" ) }
${ var ROOT = path.dirname( module.filename ) }

Scheme/JavaScript interface
===========================

${ <span class="label label-warning">Note:</span> } This document
describes the interface between JavaScript and Hop, the Scheme web
extension. This interface is subject to changes and no there is no
guarantee that the interface will be preserved across different
development branches, although effort will be accomplished to support
backward compatibility.


Scheme inline
-------------

Scheme and JavaScript use two different identifier spaces but Scheme
identifiers can be used inside JavaScript code using the following
dedicated syntactic extension:

```ebnf
${ doc.include( ROOT + "/scheme.bnf" ) }
```

Scheme variables and Scheme functions can be used inrestrictly inside
JavaScript code. The following example constructs a Scheme list of three
elements, modifies the first, and fetches the second:

```hopscript
let x = #:list( 1, 2, 3 );
#:set-car!( x, 0 );
let y = #:cadr( x );
```


Modules
-------

There is no relationship between Scheme modules and JavaScript modules
but the JavaScript `require` form can be used to _import_ Scheme
programs into JavaScript. For that the Scheme module **must** export
a function named `hopscript` that should accept exactly for arguments:

  1. `%this`, the global JavaScript created for that module;
  2. `${<span>_</span>}`,  currently unused;
  3. `scope`, the scope module object. The scope object contains all the
 JavaScript module global variables;
  4. The `module` JavaScript object.


When the a required module is suffixed with `.hop` the following actions
are executed:

  1. the Scheme module is loaded in memory;
  2. a fresh scope JavaScript object and a fresh JavaScript module object
 are created;
  3. the Scheme function `hopscript` is invoked, passing it the newly created
 object.


The result of the `require` form is the freshly allocated module object.

Example:

First, let us consider the JavaScript module importing the Scheme module
`example.hop`:

```hopscript
const hopmod = require( "./example.hop" );
console.log( hopmod );
```

and let us define a simple example that merely leaves a trace of its loading:
```hop
(module a-module
   (export (hopscript %this _ scope mod)))

(define (hopscript %this _ scope mod)
   (print "in a-module"))
```
   

Accessing JavaScript Objects
----------------------------

JavaScript object attributes are read and written from Scheme using the
getter and setter described here:

### (js-get jsobject propname %this) ###
[:@glyphicon glyphicon-tag scheme]

### (js-put! jsobject propname throw %this) ###
[:@glyphicon glyphicon-tag scheme]

The first argument `jsobject` is the JavaScript object. The second argument
`propname` is the name of the read property. It is a Scheme symbol. The
third argument of the `js-put!` function is a boolean that control whether
an exception should be raised if the property cannot be writen. The
last argument is a global JavaScript, as one received on module initialization.

Example:

Let us defines a Scheme module that exports the JavaScript variable `loaded`.

```hop
(module a-module-with-binding
  (export (hopscript %this _ scope mod)))

(define (hopscript %this _ scope mod)
   (let ((exports (js-get mod 'exports %this)))
      (js-put! exports 'loaded #t #f %this)))
```

This module can be used within JavaScript code as:

```hopscript
console.log( require( "./mod.hop" ).loaded );
```

See Section [JavaScript Object Implementation](#generic) for the complete
definitions of `js-get` and `js-put!`.


Value Mapping
-------------

This section described the mapping between JavaScript and Scheme values.

### Boolean ###

JavaScript and Scheme booleans are represented similary. No
conversion is then needed but many values represents a _false_ value in
a test. The JavaScript test conversion can be obtained from Scheme with
the following function:

#### (js-totest value) ####
[:@glyphicon glyphicon-tag scheme]


### Null and Undefined ###

JavaScript `null` is represented as the Scheme empty list `()`. JavaScript
`undefined` is represented as the Scheme `unspecified` value.


### Strings ###

JavaScript strings are internally, _i.e._, in Scheme, represented as
8-bit ropes. Their representations depend on the characters they contain
and on the way they are built. Two functions convert strings from
JavaScript to Scheme and the other way around.

#### (js-jsstring->string jsstring) ####
[:@glyphicon glyphicon-tag scheme]

Converts a JavaScript strings into an 8-bits Scheme Scheme. This returns
the UTF-8 representation of the UCS2 JavaScript strings.


#### (js-string->jsstring string) ####
[:@glyphicon glyphicon-tag scheme]

Converts a 8-bits Scheme string into a JavaScript string. The Scheme string
must be an UTF-8 encoding of the produced JavaScript string.


### Numbers ###

JavaScript numbers are implemented as Scheme integers and Scheme reals.
No conversion is needed from JavaScript to Scheme. Conversions are needed
from Scheme to JavaScript. The function `js-number->jsnumber` does it.


#### (js-number->jsnumber number) ####
[:@glyphicon glyphicon-tag scheme]

Converts a Scheme number into a JavaScript number.



JavaScript Object Implementation
--------------------------------
[:generic]

JavaScript allocated objects are instances of a subclass of
`JsObject`.  Global objects (the `%this` parameter of module
declaration) are instances of the `JsGlobalObject` class. These
classes own private fields that should not be directly accessed.

#### (generic js-tostring ::JsObject ::JsGlobalObject) ####
[:@glyphicon glyphicon-tag scheme]

This generic function is invoked to convert a JavaScript object into a
JavaScript string.

#### (generic js-tonumber ::JsObject ::JsGlobalObject) ####
[:@glyphicon glyphicon-tag scheme]

This generic function is invoked to convert a JavaScript object into a
JavaScript number.

#### (generic js-integer ::JsObject ::JsGlobalObject) ####
[:@glyphicon glyphicon-tag scheme]

This generic function is invoked to convert a JavaScript object into a
JavaScript integer.

#### (generic js-valueof ::JsObject ::JsGlobalObject) ####
[:@glyphicon glyphicon-tag scheme]

This generic function implements the `valueOf` JavaScript method of
builtin objects.

#### (generic js-get ::JsObject ::symbol ::JsGlobalObject) ####
[:@glyphicon glyphicon-tag scheme]

This generic function is used to fetch object attributes.

#### (generic js-put! ::JsObject ::symbol ::obj ::bool ::JsGlobalObject) ####
[:@glyphicon glyphicon-tag scheme]

This generic function is used to set object attributes.


### JsWrapper ###

The `JsWrapper` class is to be used by Scheme programs that need to pass
opaque values to JavaScript, that JavaScript is expected to return to
Scheme code.

#### (class JsWrapper::JsObject obj data) ####
[:@glyphicon glyphicon-tag scheme]

This class can be sub-classes, in particular to override the generic
functions.
