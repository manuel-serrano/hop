Hop: a multitier JavaScript
---------------------------

Hop is:

* A compliant JavaScript implementation:
  - EcmaScript 5.1 compliant;
  - EcmaScript 6 features;
  - Nodejs compatibility.
* An extended JavaScript:
  - **multitier**: a single code that runs on the client;
  - builtin **HTML**.
* A builtin multithreaded **Web server**.

Hop programs are executed in the context of a builtin Web server. Services
are an extension of plain JavaScript functions that get automatically invoked
when HTTP request are received by the server. Answering a request only
takes defining a `service`:

```hopscript[:prog1@homeprog]
service hello() {
  return "hello world";
}
```

To run the server put this code in the file `hello.js` and run Hop with:

```sh[:shell@homeprog]
hop -p 8080 hello.js
```

Hop extends JavaScript with the geniune HTML syntax such as
`<div>`, `<span>`, or `<button>`. The service can be
modified to return a HTML document:

```hopscript[:prog2@homeprog]
service hello() {
  return <html><div>hello world</div></html>;
}
```

Hop is multitier: client-side codes are also implemented in Hop. The
`~{` mark switches JavaScript from server-side code to client-side code:

```hopscript[:prog3@homeprog]
service hello() {
  return <html><div onclick=~{ alert( "world" ) }>hello</html>;
}
```

Hop client-side code and server-side can be mixed using the
`\${` mark:

```hopscript[:prog4@homeprog]
service hello( { who: "foo" } ) {
  return <html><div onclick=~{ alert( "Hi " + ${who} + "!") }>hello ${who}</html>;
}
```
  
Additional examples can be found in the `examples` directory of the
[source code](https://github.com/manuel-serrano/hop/tree/3.0.x/examples).
