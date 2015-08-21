This example shows how to use *asynchronous responses*. Asynchronous
responses are needed, when a service cannot respond instantly to
a request. This happens when the service execution relies on a
asynchronous computation, which can either results from a remote service
invocation or an asynchronous function call.

This example uses the standard `fs.readFile` function which is
asynchronous. It returns instantly a registers a callback function
that is to be called when the characters of the file are all read.

Asynchronous responses are implemented by ECMAScript 6 promises. That
is, when a service returns a promise, this promise is treated by the
server as an asynchronous response. When `executor` of the promise
_resolves_, the resolved value is transmitted to the client.

In the example, once the characters are read, they are fontified
using the hop `hop.fontifier` module. Then an Html document is
built, which is eventually shipped to the client using the
`resolve` function.
