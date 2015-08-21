This example illustraed service declaration with fixed number
of argument and asynchronous responses.

The service `foo` must call the service `bar` on the same host.
Calling the serving synchronously would result in a dead lock
as only **one** thread is in charge of handling services.
The call to `bar` must then be asynchronous. This is acheived
by applying the `post` method of the frame computed with
`bar( x + 1 )`.

The service `foo` relies on a asynchronous computation. It then cannot
respond immediately to the client. It will be in position to
reply only when the invocation of `bar` has completed. This is
implemented using a `hop.HTTPResponseAsync` object. The argument
`sendReponse` is a function automatically created by the runtime
system. When invoked with `sendReponse( e )` the result of the
service `bar` is replied to the cilent which has called `foo`.
