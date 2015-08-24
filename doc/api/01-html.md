${ var doc = require( "hopdoc" ) }
${ var fontifier = require( "fontifier" ) }

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

 * `require`: a string or an array of strings. The list of modules that
 can be _required_ in client side code.
 * `jscript`: a string or an array of strings. The list of client-side scripts
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
-----------

In addition to standard HTML5 tags, Hop.js supports the following tags.

### <SVG:IMG [attributes]> ###
[:@glyphicon glyphicon-tag tag]

An SVG Image. The attributes are:

 * `src`: the URL of the SVG image.
 * `width`: the width of the image.
 * `height`: the height of the image.

${ doc.include( doc.EXAMPLES_DIR + "/svg/README.md" ) }

${ <span class="label label-info">svg/svg.js</span> }

```hopscript
${ doc.include( doc.EXAMPLES_DIR + "/svg/svg.js", 13 ) }
```



