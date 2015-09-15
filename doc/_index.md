Hop: a multitier JavaScript
---------------------------

Hop is:

* A compliant JavaScript implementation:
  - EcmaScript 5.1 compliant;
  - EcmaScript 6 features;
  - Nodejs compatibility.
* An extended JavaScript:
  - **multitier**: a single code that runs on the client and the server;
  - builtin **HTML**.
* A builtin multi-threaded **web server**.

Hop programs execute in the context of a builtin web server. They
define services, which are extended JavaScript functions that get
automatically invoked when HTTP requests are received by the
server. Functions and services are syntactically similar but the latter
are defined using the `service` keyword:

```hopscript[:prog1@homeprog]
service hello() {
  return "hello world";
}
```

To run this program put this code in the file `hello.js` and run Hop with:

```sh[:shell@homeprog]
hop -p 8080 hello.js
```

You can now browse ${<tt>http://localhost:8080/hop/hello</tt>}.


Hop extends JavaScript with the geniune HTML syntax such as
`<div>`, `<span>`, or `<button>`. Our service can be
modified to return a HTML document:

```hopscript[:prog2@homeprog]
service hello() {
  return <html><div>hello world</div></html>;
}
```

Hop is multitier. That is client-side codes are also implemented in Hop. The
`\~{` mark switches JavaScript from server-side code to client-side code:

```hopscript[:prog3@homeprog]
service hello() {
  return <html><div onclick=~{ alert( "world" ) }>hello</html>;
}
```

Hop client-side code and server-side can be mixed using the
`\${` mark:

```hopscript[:prog4@homeprog]
service hello( { who: "foo" } ) {
  return <html><div onclick=~{ alert( "Hi " + ${who} + "!") }>hello</html>;
}
```

Many additional examples can be found in the
[source development tree](https://github.com/manuel-serrano/hop/tree/3.0.x/examples).

By default Hop.js only accepts to serve authenticated requests. Before
executing any programs _users_ must be declared. These declarations go
into the `$HOME/.config/hop/hoprc.js` file. The following declare a
user named `hopjs` whose password is `inria` and that is allowed to
execute any Hop.js service ( `services: "*"`) and download any file
readable from the server process ( `directories: "*"`):

```sh[:config@homeprog]
mkdir -p $HOME/.config/hop && cat > $HOME/.config/hop/hoprc.js << EOF
hop = require( "hop" );
var user = require( hop.user );
var config = require( hop.config );

user.add( { name: "hopjs",
            password: user.encryptPassword( "hopjs", "inria" ),
            services: "*",
            directories: "*"
          } );
EOF
```

You are now ready to execute Hop.js programs!
