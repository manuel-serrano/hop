Modules can be required by either server-side source code and client-side
source code. This example shows this latter possibility. The module
`mod1.js` and `mod2.js` are used by document constructued by the
service `requirec` but only the module `mod1.js` is explicitly required
by the HTML document. The module `mod1.js` requires the module `mod2.js`.
In this cases, both modules have to be mentionned in the `require` attribute
of the `head` element of the main document.
