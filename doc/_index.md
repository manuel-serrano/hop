Hop.js: a multitier JavaScript
------------------------------

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
  - native **websockets**.
* A builtin **multi-threaded** web server.

Hop also supports the 
[Scheme](http://www-sop.inria.fr/indes/fp/Bigloo/doc/r5rs.html) 
programming language. With Hop.js, JavaScript and Scheme are fully 
interoperable and applications can mix both language. This
page mostly describes the JavaScript layer. The Scheme layer
is described on a dedicated [page](http://hop.inria.fr/hop/doc?lang=hop).

Hop programs execute in the context of a builtin web server. They
define services, which are _super_ JavaScript functions that get
automatically invoked when HTTP requests are received. Functions and
services are almost syntactically similar but the latter are defined using
the `service` keyword:

```hopscript[:prog1@hopscript]
service hello() {
  return "hello world";
}
```

To run this program put this code in the file `hello` and execute:

```sh[:@shell]
$ hop -p 8080 hello.js
```

You can now browse `http://localhost:8080/hop/hello`.


Hop extends JavaScript with the geniune HTML. if we want to modify
our service to make it return an HTML document, we can use:

```hopscript[:prog2@hopscript]
service hello() {
  return <html><div>hello world</div></html>;
}
```

Hop is multitier. That is client-side codes are also implemented in Hop. The
`\~{` mark switches from server-side context to client-side context:

```hopscript[:prog3@hopscript]
service hello() {
  return <html><div onclick=~{ alert( "world" ) }>hello</div></html>;
}
```

Hop client-side code and server-side can also be mixed using the
`\${` mark:

```hopscript[:prog4@hopscript]
service hello( { name: who } ) {
  return <html><div onclick=~{ alert( "Hi " + ${who} + "!") }>hello</div></html>;
}
```

By default Hop.js only accepts to serve authenticated requests. Before
executing any programs _users_ must be declared. These declarations go
into the `$HOME/.config/hop/hoprc.js` file. The following declare a
user named `hopjs` whose password is `inria` and that is
allowed to execute any Hop.js service, the declaration `services: "\*"`, and
download any file readable from the server process, the declaration
`directories: "\*"`:

```sh[:config@config]
$ mkdir -p $HOME/.config/hop && cat > $HOME/.config/hop/hoprc.js << EOF
hop = require( "hop" );
var user = require( hop.user );
var config = require( hop.config );

user.add( { name: "hopjs",
            password: user.encryptPassword( "hopjs", "inria" ),
            services: "*",
            directories: "*"
          } );
user.add( { name: "anonymous",
            password: user.encryptPassword( "hopjs", "23^73++_*" ),
            services: "",
            directories: ""
          } );	  
EOF
```

You are now ready to execute Hop.js programs! Many more additional examples
can be found in the
[source development tree](https://github.com/manuel-serrano/hop/tree/3.1.x/examples).
