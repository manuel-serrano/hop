
${var hop = require( "hop" )}


### Hop command ###

    ,(html-string-encode (system->string (format "~a/hop --help" (hop-bin-directory))))


### Unix environments ###

The directory `ROOT/arch` contains various configurations for Linux
systems, Android, and MacOS X system. In particular, it contains
scripts for generating Hop pakcages for Arch Linux and Debian. This
scripts might as a documentation for understanding how to spawn Hop
automatically at boot time and how to run a safe Hop server to accepts
connection on the port `80`. The most relevant files for this are:

  * arch/archlinux/rc.d/hop.in
  * arch/debian/init.d
