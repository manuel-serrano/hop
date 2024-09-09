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
* Two implementations:
  1. A Nodejs embedding (i.e., Hop.js can be installed inside Nodejs via an NPM package).
  2. A builtin **multi-threaded** web server.

> [!NOTE]
> Hop.js also supports the [Scheme](http://www-sop.inria.fr/indes/fp/Bigloo/)
> programming language. When using the nativer Hop.js version (as opposed
> to the Nodejs hostsed version), JavaScript and Scheme are fully 
> interoperable and applications can mix both languages. This
> page mostly describes the JavaScript layer. The Scheme layer is
> described in a dedicated
> [programming manual](http://hop.inria.fr/hop/doc?lang=hop).

Hop.js programs execute in the context of a builtin web server. They
define services, which are _super_ JavaScript functions that get
automatically invoked when HTTP requests are received. As service
associates an URL to a JavaScript and enables calls by the means
of HTTP requests:

```javascript
// hello.hop.mjs
import { Hop } from "@hop/hop";

const hop = new Hop({ports: {http: 8888}});

function hello(o) {
  return "hello world";
}

const Hello = hop.Service(hello);
hop.listen().then(() => console.log(`${Hello()} ready...`);
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


Hop.js extends JavaScript with the geniune HTML language. if we want to modify
our service to make it returns an HTML document, we can use:

```javascript
function hello() {
  return <html><div>hello world</div></html>;
}
```

Hop.js is multitier. That is client-side codes are also implemented in Hop. The
`~{` mark switches from server-side context to client-side context:

```javascript
function hello() {
  return <html><div onclick=~{alert("world")}>hello</div></html>;
}
```

Hop.js client-side code and server-side can also be mixed using the
`\${` mark:

```javascript
service hello({ name: who }) {
  return <html><div onclick=~{alert("Hi " + ${who} + "!")}>hello</div></html>;
}
```

By default Hop.js only accepts to serve authenticated requests. Users
must be declared when instantiating the server. The following declares
two users. First, a user named `hopjs` whose password is `abcdef`, and
that is allowed to execute any Hop.js service (the declaration
`services: "\*"`) and download any file readable from the server
process (the declaration `directories: "\*"`). Second, the "anonymous"
user, which is associated to unauthenticated requests. No password is
associated with that user. He is only allowed to run a `wizard`
service and is only allowed to access the Hop.js runtime files.

```javascript
import { Hop } from "@hop/hop";

const users = [ {
      name: "hopjs",
      password: Hop.passwordEncrypt("hopjs", "abcdef"), // should be precomputed with "hopuser.mjs"
      services: "*",
      directories: "*"
   }, { 
      name: "anonymous",
      services: ["wizard"],
      directories: hop.loadPath
   } ];
   
const hop = new Hop({ports: {http: 8888}, users});
```

> [!CAUTION]
> Of course, passwords should not be included as plain texts in
> programs. Instead they should be precomputed and only the result of the
> encryption should be included in the program. The Hop.js distribution
> provided the `bin/hopuser.mjs` that generates user declarations
> with an encrypted password.

You are now ready to execute Hop.js programs! Many more additional examples
can be found in the
[source development tree](https://github.com/manuel-serrano/hop/tree/3.1.x/examples).

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
[[main page]](../README.md) | [[hello world]](./hello.md) | [[language]](./lang/README.md) | [[license]](./license.md)
