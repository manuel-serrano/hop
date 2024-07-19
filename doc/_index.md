![http://hop.inria.fr](./logo.svg) Hop.js: a multitier JavaScript
=================================================================

Hop.js (aka Hop) is:

* A multitier JavaScript:
  - a single code runs on the client and the server.
* A compliant JavaScript implementation:
  - EcmaScript 5.1 compliant;
  - EcmaScript 6 features (modules, `async`/`await`, proxy objects, ...);
  - Nodejs compatibility.
* An extended JavaScript:
  - builtin **HTML**: native HTML syntax support and multitier dom.
  - server-side web **workers**.
  - **websockets**.
* A builtin web server.

${<span class="label label-warning">Note:</span>}
Hop also supports the [Scheme](http://www-sop.inria.fr/indes/fp/Bigloo/)
programming language. When using the nativer Hop.js version (as opposed
to the Nodejs hostsed version), JavaScript and Scheme are fully 
interoperable and applications can mix both languages. This
page mostly describes the JavaScript layer. The Scheme layer is
described in a dedicated
[programming manual](http://hop.inria.fr/hop/doc?lang=hop).

Hop programs execute in the context of a builtin web server. They
define services, which are _super_ JavaScript functions that get
automatically invoked when HTTP requests are received. As service
associates an URL to a JavaScript and enables calls by the means
of HTTP requests:

```javascript
import { Hop } from "@hop/hop";

const hop = new Hop({ports: {http: 8888}});

function hello(o) {
  return "hello world";
}

const Hello = hop.Service(hello);
hop.listen().then(() => console.log(`${Hello()} ready...);
```

To run this program put this code in the file `hello.hop.mjs`, compile it

```shell
$ hopc.mjs hello.hop.mjs -o hello.mjs
```

and execute it:

```shell
$ nodejs hello.mjs
```

You can now browse `http://localhost:8888/hello`.


Hop extends JavaScript with the geniune HTML. if we want to modify
our service to make it return an HTML document, we can use:

```javascript
function hello() {
  return <html><div>hello world</div></html>;
}
```

Hop is multitier. That is client-side codes are also implemented in Hop. The
`~{` mark switches from server-side context to client-side context:

```javascript
function hello() {
  return <html><div onclick=~{alert("world")}>hello</div></html>;
}
```

Hop client-side code and server-side can also be mixed using the
`\${` mark:

```javascript
service hello({ name: who }) {
  return <html><div onclick=~{ lert("Hi " + ${who} + "!")}>hello</div></html>;
}
```

By default Hop.js only accepts to serve authenticated requests. Users
must be declared when creating the server. The following declare a
user named `hopjs` whose password is `inria` and that is
allowed to execute any Hop.js service, the declaration `services: "\*"`, and
download any file readable from the server process, the declaration
`directories: "\*"`:

```javascript
import { Hop } from "@hop/hop";

const users = [ {
      "name": "hopjs",
      "password": "ENCRYPTED-PASSWORD",
      services: "*",
      directories: "*"
   }, { 
      name: "anonymous",
      services: ["wizard"],
      directories: hop.loadPath
   } ];
   
const hop = new Hop({ports: {http: 8888}, users});
```

You are now ready to execute Hop.js programs! Many more additional examples
can be found in the
[source development tree](https://github.com/manuel-serrano/hop/tree/3.1.x/examples).

 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
[[main page]](../README.md) | [[hello world]](./hello.md) | [[language]](./lang/README.md) | [[license]](./license.md)
