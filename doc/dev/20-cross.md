${var hop = require( "hop" )}
${var hopdoc = require( "hopdoc" )}


### Cross-compiling Hop ###

Cross-compiling Hop requires first to prepare a cross-capable version
of the Bigloo compiler. The procedure is described in the "Cross
Compilation" section of the Bigloo manual. The distribution also
contains specific documentations for cross-compiling Bigloo for
raspberry and android platforms.

In addition to that specific version, cross-compiling Hop also requires
a standard Bigloo version and a standard Hop version. Assuming that these
are installed in the `/usr/local/bin/` directory and that the cross
compiler version is install in the `/opt/cross` directory,
Hop must be configured with:

  ./configure \
     --bigloo=/usr/local/bin/bigloo \
     --bigloolibdir=/opt/cross/lib/bigloo/4.3b \
     --hopc=/usr/local/bin/hopc \
     --hop=/usr/local/bin/hop

Then Hop must be compiled and installed using the regular method:

  make && make install


