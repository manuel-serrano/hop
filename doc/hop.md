Hop: a multitier JavaScript
---------------------------

* Full JavaScript compatibility (EcmaScript 5.1, Nodejs, some EcmaScript 6);
* Multitier: a single JavaScript code that runs on the client;
* HTML builtin;
* Server support for
  - web **workers**;
  - websockets.


Example: minimalist web server
------------------------------

```hopscript
service hello() {
  return <html><div onclick=~{alert("world")}>hello</html>;
}
```

