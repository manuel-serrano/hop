![http://hop.inria.fr](./logo.svg) Hop --- Hello World
=============================================================================

The following example illustrates the Hop programming flavor:

<span class="hiphop">&#x2605;</span> Example: [ls.hop.mjs](../examples/ls/ls.hop.mjs)
<!-- ${doc.includeCode("../examples/ls/ls.hop.mjs")} -->

  * The first three lines are regular JavaScript module imports. Note that
Hop imposes to use ECMAScript modules on the server-side and
client-side of the application.

  * The variable `hopConfig` holds the server configuration used for that
example. It will expect http connection on the port `8888` and it will
allow unauthentified users to execute the service named `ls`;

  * The variable `hop` is bound to the running Hop server and the variable
`R` is bound to a _static resolver_ of that service. It will be used
to enable the server to delivery static page.

  * The asynchronous function `ls` is the implementation of the unique
service of this example. It takes an object as parameter that contains
the name of the directory to be scaned. Using the Nodejs' asynchronous API
it reads files on the disk and it builds the result as a web page.
Inside an HTML fragments the syntax `${...}` is similar to that of
JavaScript string patterns. It insert the rsult of the evaluation in the
HTML fragment that is finally assembled.
Hop uses an extension to HTML to denote attributes that hold client
side code. This is the `~{...}` syntax. Inside a client code, the
pattern `${...}` is another sort of hole. It denotes a server side expression.

  * The variable `Ls` is the Web service. The implementation is the function
  `ls`. As no URL is specified when the service is created, it takes its name
  from the JavaScript function. The URL for that service is then `http://localhost:8888/ls`.
  
  * Finally, the last line of the program starts the web server.


Installation
------------

A Hop program can be executed in a regular Node.js environment or in a
native version. For the sake of simplicity we assume in this example
an execution within Node.js.

To install the Hop dependencies to run this program, use the following
`package.json` file:

```json
{
   "name": "ls",
   "dependencies": {
     "@hop/hop": "file:/home/serrano/www/public_html/software/npmx/hop.tgz",
     "@hop/hopc": "file:/home/serrano/www/public_html/software/npmx/hopc.tgz"
   },
   "scripts": {
      "build": "./node_modules/@hop/hopc/bin/hopc.mjs ls.hop.mjs -o ls.mjs",
      "run": "node --enable-source-maps ls.mjs"
   }
}
```

and in the same directory

```shell
npm install
npm build
```

Finally, to run the program:

```shell
npm run
```

 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
[[main page]](../README.md) | [[hello world]](./hello.md) | [[language]](./lang/README.md) | [[license]](./license.md)
