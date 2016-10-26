
Compatibility
=============

Hop.js supports the following Node.js modules:

* [Buffer](https://nodejs.org/api/buffer.html)
* [Child Processes](https://nodejs.org/api/child\_process.html)
* [Cluster](https://nodejs.org/api/cluster.html)
* [Console](https://nodejs.org/api/console.html)
* [Crypto](https://nodejs.org/api/crypto.html)
* [DNS](https://nodejs.org/api/dns.html)
* [Domain](https://nodejs.org/api/domain.html)
* [Events](https://nodejs.org/api/events.html)
* [File System](https://nodejs.org/api/fs.html)
* [Globals](https://nodejs.org/api/globals.html)
* [HTTP](https://nodejs.org/api/http.html)
* [HTTPS](https://nodejs.org/api/https.html)
* [Modules](https://nodejs.org/api/modules.html)
* [Net](https://nodejs.org/api/net.html)
* [OS](https://nodejs.org/api/os.html)
* [Path](https://nodejs.org/api/path.html)
* [PROCESS](https://nodejs.org/api/process.html)
* [Punycode](https://nodejs.org/api/punycode.html)
* [Query Strings](https://nodejs.org/api/querystring.html)
* [Stream](https://nodejs.org/api/stream.html)
* [String Decoder](https://nodejs.org/api/string\_decoder.html)
* [Timers](https://nodejs.org/api/timers.html)
* [TLS/SSL](https://nodejs.org/api/tls.html)
* [TTY](https://nodejs.org/api/tty.html)
* [UDP/Datagram](https://nodejs.org/api/udp.html)
* [URL](https://nodejs.org/api/url.html)
* [Utilities](https://nodejs.org/api/util.html)
* [VM](https://nodejs.org/api/vm.html)

Notable differences
==============

The default behaviour of Hop.js is to launch an HTTP server. To mimic
Node.js, use the `--no-server` option to disable the launch of the
Hop.js built-in server, and automatically terminate the process when
all events are processed.

Hop.js `eval` function does not access the lexical scope surrounding
the `eval`call.  For Example:

```hopscript var x = true;
function foo() {
  var x = false;
  return( eval( 'x' ));
  }

console.log( foo() );
```

`foo()` would return `false`with Node.js, and `true`with Hop.js.

