This example shows how to implement an external Hopc compilation stage
that modifies the ast it receives (by replacing `+` with `*`) and by
displaying some information about the program.

To execute this example, a server must be executed with:

```shell
% hop -v -g -p 8888 js2http.js
```

The compiler should be invoked with:

```shell
% hopc -v fact.js --js-driver syntax,hopscript-header, \
   loopexit,bestpractice,symbol,this,read-only,return, \
   property,http://localhost:8888/hop/js2http, \
   scheme
% ./a.out
```
