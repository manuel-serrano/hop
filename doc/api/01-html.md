${ var doc = require( "hopdoc" ) }

HTML
====

HTML tags are implemented as builtin HopScript functions. When using
the HTML syntax, identifiers are case-insensitive. For instance,
`<DIV>`, `<div>`, and `<Div>` are equivalent.


HTML5 tags
----------

HTML tags have the orginal [HTML5](http://www.w3.org/TR/html5/index.html#elements-1) meaning. Some tags have additional attributes that are for Hop.js
use only. These are described in this document.

### <HEAD> ###
[:@glyphicon glyphicon-tag tag]

Hop.js extends the HTML5 `<head>` attributes with the following list:

 * `module`: a string or an array of strings. The list of modules that
 can be _required_ in client side code. Requiring a module from a client-side
 code not listed in this list raises a runtime error.
 * `script`: a string or an array of strings. The list of client-side scripts
 that are used by the document. This attribute is automatically expanded
 into the correspondant list of `<SCRIPT>` nodes.
 * `css`: a string or an array of strings. The list of Hop.js Cascading Style
 sheets that are used by the document. This attribute is automatically expanded
 into the correspond list of `<LINK>` nodes.
 * `rts`: a boolean. When `false` disables the automatic inclusion of the
 Hop.js client-side runtime in the document. If `rts` is `true` or not mentionned
 the runtime is included.
 * `favicon`: a string. The URL of the document favicon, a shorthand for `<link>`
 favicon.
 * `title`: a string. The title of the document, equivalent to a `<title>`
 node.
 * `inline`: a boolean. When `true` all resources are inlined in the generated
 document, which is then standlone.


Example

```hopscript
function foo( title ) {
  return <head title=${title} css=${PATH + "/my-css.hss"} inline=${true}>;
}
```

### <IMG> ###
[:@glyphicon glyphicon-tag tag]

Hop.js can automatically inline images, which might be used to generate
self-contained HTML documents. For that is supports the extra `inline`
property.

 * `inline`: a boolean, When `true` the content of the image is inline
 in the node using a base64 representation of the image bytes.


Hop.js tags
===========

In addition to standard HTML5 tags, Hop.js supports the following tags.

### <SVG:IMG [attributes]> ###
[:@glyphicon glyphicon-tag tag]

An SVG image. The attributes are:

 * `src`: the URL of the SVG image.
 * `width`: the width of the image.
 * `height`: the height of the image.

The tag `<svg:img>` may be used everywhere in HTML documents. It should not
be used inside a `<svg>` section.

${ doc.include( doc.EXAMPLES_DIR + "/svg/README.md" ) }

${ <span class="label label-info">svg/svg.js</span> }

```hopscript
${ doc.include( doc.EXAMPLES_DIR + "/svg/svg.js", 14 ) }
```

### <MATH:TEX> ###
[:@glyphicon glyphicon-tag tag]

Parse a math formula expressed in the TeX syntax and build the corresponding
MathML DOM tree. 


SVG tags
========

Hop.js supports the following SVG tags.

### <SVG [attributes]> ###
[:@glyphicon glyphicon-tag tag]

### <SVG:DEFS [attributes]> ###
[:@glyphicon glyphicon-tag tag]

### <SVG:RECT [attributes]> ###
[:@glyphicon glyphicon-tag tag]

### <SVG:CIRCLE [attributes]> ###
[:@glyphicon glyphicon-tag tag]

### <SVG:ELLIPSE [attributes]> ###
[:@glyphicon glyphicon-tag tag]

### <SVG:FILTER [attributes]> ###
[:@glyphicon glyphicon-tag tag]

### <SVG:FEGAUSSIANBLUR [attributes]> ###
[:@glyphicon glyphicon-tag tag]

### <SVG:FECOLORMATRIX [attributes]> ###
[:@glyphicon glyphicon-tag tag]

### <SVG:FOREIGNOBJECT [attributes]> ###
[:@glyphicon glyphicon-tag tag]

### <SVG:G [attributes]> ###
[:@glyphicon glyphicon-tag tag]

### <SVG:IMG [attributes]> ###
[:@glyphicon glyphicon-tag tag]

### <SVG:LINE [attributes]> ###
[:@glyphicon glyphicon-tag tag]

### <SVG:PATH [attributes]> ###
[:@glyphicon glyphicon-tag tag]

### <SVG:POLYLINE [attributes]> ###
[:@glyphicon glyphicon-tag tag]

### <SVG:POLYGON [attributes]> ###
[:@glyphicon glyphicon-tag tag]

### <SVG:TEXT [attributes]> ###
[:@glyphicon glyphicon-tag tag]

### <SVG:TEXTPATH [attributes]> ###
[:@glyphicon glyphicon-tag tag]

### <SVG:TREF [attributes]> ###
[:@glyphicon glyphicon-tag tag]

### <SVG:TSPAN [attributes]> ###
[:@glyphicon glyphicon-tag tag]


MathML tags
===========

Hop.js supports the following MathML tags.

### <MATH [attributes]> ###
[:@glyphicon glyphicon-tag tag]

### <MATH:MSTYLE [attributes]> ###
[:@glyphicon glyphicon-tag tag]

### <MATH:MI [attributes]> ###
[:@glyphicon glyphicon-tag tag]

### <MATH:MN [attributes]> ###
[:@glyphicon glyphicon-tag tag]

### <MATH:MO [attributes]> ###
[:@glyphicon glyphicon-tag tag]

### <MATH:MROW [attributes]> ###
[:@glyphicon glyphicon-tag tag]

### <MATH:MUNDER [attributes]> ###
[:@glyphicon glyphicon-tag tag]

### <MATH:MOVER [attributes]> ###
[:@glyphicon glyphicon-tag tag]

### <MATH:MUNDEROVER [attributes]> ###
[:@glyphicon glyphicon-tag tag]

### <MATH:MSUP [attributes]> ###
[:@glyphicon glyphicon-tag tag]

### <MATH:MSUB [attributes]> ###
[:@glyphicon glyphicon-tag tag]

### <MATH:MSUBSUP [attributes]> ###
[:@glyphicon glyphicon-tag tag]

### <MATH:MFRAC [attributes]> ###
[:@glyphicon glyphicon-tag tag]

### <MATH:MROOT [attributes]> ###
[:@glyphicon glyphicon-tag tag]

### <MATH:MSQRT [attributes]> ###
[:@glyphicon glyphicon-tag tag]

### <MATH:MTEXT [attributes]> ###
[:@glyphicon glyphicon-tag tag]

### <MATH:MTABLE [attributes]> ###
[:@glyphicon glyphicon-tag tag]

### <MATH:MTR [attributes]> ###
[:@glyphicon glyphicon-tag tag]

### <MATH:MTD [attributes]> ###
[:@glyphicon glyphicon-tag tag]

### <MATH:MPADDED [attributes]> ###
[:@glyphicon glyphicon-tag tag]


Tilde
-----

### new Tilde( string ) ###
[:@glyphicon glyphicon-tag constructor]

Constructs a new client-side program. This constructor is to used to
generate client-side programs whose contained are obtained by a
computation. In most situations, one should normaly prefer using
the `\~` syntax.
