${ var doc = require( "@hop/hopdoc" ) }

HTML
====

HTML tags are implemented as builtin HopScript functions. When using
the HTML syntax, identifiers are case-insensitive. For instance,
`DIV`, `div`, and `Div` are equivalent.


Local URL
---------

URL are used inside HTML documents to refer to external resources. For 
instance, the `src` attribute of the `img` tag refers to the URL of the 
image to be displayed or the `src` attribute of the  `audio` tag refers
to the URL of the music to be played. These URL might be located on another
server, in which case, the `http://` schema is used. They might also
be located on the same server as the one delivering the page. In that
particular case, the URL designate a file and the URL might either be
absolute, that is the filename starts with the `/` character or relative,
that is relative to the URL of the current document. In the following, we
propose two ways for constructing URLS.

The easiest way to build an _absolute_ URL is to rely on the
`require.resolve` function to produce an absolute path. This function
constructs an absolute path name of a file whose actual path is relative
to the JavaScript source code that disignates it. Using `require.resolve`
ensures that if the whole source code of the application is moved into
another directory, the absolute path of the HTML resource will remains
correct.

Using an absolute URL has the drawback of exposing the directory hierarchy
of the server disk. This is generally unaccaptable for production servers.
Unfortunately, relative URL can generaly not be used because the URL forged
to represent services that are prefixed with `/hop` do not correspond to
actual disk directories. The easiest workaround consists in defining another
service whose only purpose is to serve local file and then to create the
URL by invoking that service.

Example:

```hopscript
${ doc.include( doc.EXAMPLES_DIR + "/resource/resource.js", 14 ) }
```

In this example, the first image URL is absolute. It will refer to the actual
path on the server disk of the image to be rendered. The second image
URL uses the service `getResource`. The expression `getResource( "./hop.png" )`
builds the URL corresponding to the invocation of the service with a relative
file name. The implementation of that service merely consists in building
the absolute path an returning that very file.


HTML5 tags
----------

HTML tags have the orginal [HTML5](http://www.w3.org/TR/html5/index.html#elements-1) meaning. Some tags have additional attributes that are for Hop.js
use only. These are described in this document.

### HEAD ###
[:@glyphicon glyphicon-tag tag]

Hop.js extends the HTML5 `head` attributes with the following list:

 * `script`: a string or an array of strings. The list of client-side scripts
 that are used by the document. This attribute is automatically expanded
 into the correspondant list of `SCRIPT` nodes.
 * `css`: a string or an array of strings. The list of Hop.js Cascading Style
 sheets that are used by the document. This attribute is automatically expanded
 into the correspond list of `LINK` nodes.
 * `rts`: a boolean. When `false` disables the automatic inclusion of the
 Hop.js client-side runtime in the document. If `rts` is `true` or not mentionned
 the runtime is included.
 * `favicon`: a string. The URL of the document favicon, a shorthand for `link`
 favicon.
 * `title`: a string. The title of the document, equivalent to a `title`
 node.
 * `inline`: a boolean. When `true` all resources are inlined in the generated
 document, which is then standone.


#### Example ####

A basic example.

```hopscript
function foo( title ) {
  return <head title=${title} css=${PATH + "/my-css.hss"} inline=${true}>;
}
```

### SCRIPT ###
[:@glyphicon glyphicon-tag tag]

Hop.js extends the HTML5 `script` with two attributes:

 * `module`: When used in conjunction with a `src` attrbute, this tells
 Hop.js that this module is to be used as a client-side module. The string
 used in the `src` attribute, can then be used, in client-side code,
 to _require_ client-side modules. See [LANG Modules](01-module.html) for
 details.
 * `inline`: a boolean. When `true` the script is inlined in the generated
 document.
 

### IMG ###
[:@glyphicon glyphicon-tag tag]

Hop.js can automatically inline images, which might be used to generate
self-contained HTML documents. For that is supports the extra `inline`
property.

 * `inline`: a boolean, When `true` the content of the image is inline
 in the node using a base64 representation of the image bytes.


Hop.js tags
===========

In addition to standard HTML5 tags, Hop.js supports the following tags.

### SVG:IMG [attributes] ###
[:@glyphicon glyphicon-tag tag]

An SVG image. The attributes are:

 * `src`: the URL of the SVG image.
 * `width`: the width of the image.
 * `height`: the height of the image.

The tag `svg:img` may be used everywhere in HTML documents. It should not
be used inside a `svg` section.

${ doc.include( doc.EXAMPLES_DIR + "/svg/README.md" ) }

${ <span class="label label-info">svg/svg.js</span> }

```hopscript
${ doc.include( doc.EXAMPLES_DIR + "/svg/svg.js", 14 ) }
```

### MATH:TEX ###
[:@glyphicon glyphicon-tag tag]

Parse a math formula expressed in the TeX syntax and build the corresponding
MathML DOM tree. 


SVG tags
========

Hop.js supports the following SVG tags.

### SVG [attributes] ###
[:@glyphicon glyphicon-tag tag]

### SVG:DEFS [attributes] ###
[:@glyphicon glyphicon-tag tag]

### SVG:RECT [attributes] ###
[:@glyphicon glyphicon-tag tag]

### SVG:CIRCLE [attributes] ###
[:@glyphicon glyphicon-tag tag]

### SVG:ELLIPSE [attributes] ###
[:@glyphicon glyphicon-tag tag]

### SVG:FILTER [attributes] ###
[:@glyphicon glyphicon-tag tag]

### SVG:FEGAUSSIANBLUR [attributes] ###
[:@glyphicon glyphicon-tag tag]

### SVG:FECOLORMATRIX [attributes] ###
[:@glyphicon glyphicon-tag tag]

### SVG:FOREIGNOBJECT [attributes] ###
[:@glyphicon glyphicon-tag tag]

### SVG:G [attributes] ###
[:@glyphicon glyphicon-tag tag]

### SVG:IMG [attributes] ###
[:@glyphicon glyphicon-tag tag]

### SVG:LINE [attributes] ###
[:@glyphicon glyphicon-tag tag]

### SVG:PATH [attributes] ###
[:@glyphicon glyphicon-tag tag]

### SVG:POLYLINE [attributes] ###
[:@glyphicon glyphicon-tag tag]

### SVG:POLYGON [attributes] ###
[:@glyphicon glyphicon-tag tag]

### SVG:TEXT [attributes] ###
[:@glyphicon glyphicon-tag tag]

### SVG:TEXTPATH [attributes] ###
[:@glyphicon glyphicon-tag tag]

### SVG:TREF [attributes] ###
[:@glyphicon glyphicon-tag tag]

### SVG:TSPAN [attributes] ###
[:@glyphicon glyphicon-tag tag]


MathML tags
===========

Hop.js supports the following MathML tags.

### MATH [attributes] ###
[:@glyphicon glyphicon-tag tag]

### MATH:MSTYLE [attributes] ###
[:@glyphicon glyphicon-tag tag]

### MATH:MI [attributes] ###
[:@glyphicon glyphicon-tag tag]

### MATH:MN [attributes] ###
[:@glyphicon glyphicon-tag tag]

### MATH:MO [attributes] ###
[:@glyphicon glyphicon-tag tag]

### MATH:MROW [attributes] ###
[:@glyphicon glyphicon-tag tag]

### MATH:MUNDER [attributes] ###
[:@glyphicon glyphicon-tag tag]

### MATH:MOVER [attributes] ###
[:@glyphicon glyphicon-tag tag]

### MATH:MUNDEROVER [attributes] ###
[:@glyphicon glyphicon-tag tag]

### MATH:MSUP [attributes] ###
[:@glyphicon glyphicon-tag tag]

### MATH:MSUB [attributes] ###
[:@glyphicon glyphicon-tag tag]

### MATH:MSUBSUP [attributes] ###
[:@glyphicon glyphicon-tag tag]

### MATH:MFRAC [attributes] ###
[:@glyphicon glyphicon-tag tag]

### MATH:MROOT [attributes] ###
[:@glyphicon glyphicon-tag tag]

### MATH:MSQRT [attributes] ###
[:@glyphicon glyphicon-tag tag]

### MATH:MTEXT [attributes] ###
[:@glyphicon glyphicon-tag tag]

### MATH:MTABLE [attributes] ###
[:@glyphicon glyphicon-tag tag]

### MATH:MTR [attributes] ###
[:@glyphicon glyphicon-tag tag]

### MATH:MTD [attributes] ###
[:@glyphicon glyphicon-tag tag]

### MATH:MPADDED [attributes] ###
[:@glyphicon glyphicon-tag tag]


Tilde
-----

### new Tilde( string ) ###
[:@glyphicon glyphicon-tag constructor]

Constructs a new client-side program. This constructor is to used to
generate client-side programs whose contained are obtained by a
computation. In most situations, one should normaly prefer using
the `\~` syntax.
