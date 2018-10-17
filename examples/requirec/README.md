Modules can be required by either server-side source code and client-side
source code. This example shows this latter possibility. The module
`mod1.js` and `mod2.js` are used by document constructued by the
service `requirec` but only the module `mod1.js` is explicitly required
by the HTML document. The module `mod1.js` requires the module `mod2.js`.
In this cases, both modules have to be mentionned in a `script` tag
of the `head` element of the main document.

${ <span class="label label-warning">Note:</span> } Modules mentioned
in the `head`'s `module` attribute are loaded asynchronously when the
page is created on the client. Thus they cannot be required in following
sequential code. Two options are then possible to require modules: i)
using the `require` form inside a `window.onload` callback or ii) using the
`require` form inside a `defer` script. This is the option used in
this example (note the `defer` attribute of the `head` tag). 

