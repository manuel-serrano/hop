${var hop = require( "hop" )}
${var hopdoc = require( "hopdoc" )}


https
=====

${ <span class="label label-warning">Note:</span> } The https
interface as been changed from Hop-3.2.0 and Hop-3.3.0 this document
only refers to the latter version.

Hop can wait for `http` connections, `https` connections or both. This is
decided on the command line. 

The following example, shows how to redirect incoming `http` connexion to
their `https` counterpart. In the example below if a non-`https` connection
is received and if the server session enables `https` connection, the function
return is Hop response that redirects the client to the corresponding
`http` port.


Invoking the server
-------------------

The following example shows how to run Hop with `http` connection listened
to the port `9999` and `https` connections listended to the port `8888`.

```shell
hop -p 9999 --https-port 8888 --https-pkey key.pem --https-cert cert.pem
```


Automatic Redirections
----------------------

```hopscript
function HTTPtoHTTPS( req ) {
   if( config.HTTPSPort && !req.socket.ssl ) {
      return hop.HTTPResponseString( "",
         {
            startLine: "HTTP/1.0 307 Temporary Redirect",
            header: { "location": `https://${req.host}:${config.HTTPSPort}${req.abspath}` }
         } );
   } else {
      return false;
   }
}

```
