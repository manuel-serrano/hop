This example demonstrates advanced use of JavaScript workers. The main
task is responsible to initiate two slave twin workers running the
same code, and create a browser service launch both workers in
`iframe`s.  When launched, each slave worker defines a dynamic service
and sends a service handle to the main thread to let the thread build
the main html page.

Service handles, just like other hop.js objects, can be sent as worker
messages.

Since there is a unique name space for services, all services created
in `slave.js` are anonymous. Hop ensures that unique names are
generated for each service.

Note that each `slave.js` worker operates its private counter. The same
isolation property would apply to submodules required by `slave.js`.

Note: the worker5 service cannot be invoked until both slave workers
are fully initialized, which is not an issue since the service is
invoked from the client browser only.

Putting the createWorker calls within the main service would require
an asynchronous coding style where the service implementation creates
slave workers and returns only after the worker services are
created.
