
${var hop = require( "hop" )}


### Hop command ###

    ,(html-string-encode (system->string (format "../bin/hop.sh --help")))


### Unix environments ###

The directory `ROOT/arch` contains various configurations for Linux
systems, Android, and MacOS X system. In particular, it contains
scripts for generating Hop pakcages for Arch Linux and Debian. This
scripts might as a documentation for understanding how to spawn Hop
automatically at boot time and how to run a safe Hop server to accepts
connection on the port `80`. The most relevant files for this are:

  * arch/archlinux/rc.d/hop.in
  * arch/debian/init.d


### Shebang ###

To run Hop with a shebang (`#!/...` unix directive), use the following:

```shell[:@shell]
#!/usr/bin/env -S hop --no-server --
```
