Hop Debian Package - 28 mar 2021
================================

1. To build the debian packages on the local machine
----------------------------------------------------

```shell
./makedeb.sh [-O targetdir] [--repodir dir]
```

example:

```shell
./makedeb.sh -O /tmp/debhop
```

2. To build the debian packages on a remote machine via ssh
-----------------------------------------------------------

```shell
./makedebremote.sh [-O targetdir] [host] [user]
```

Warning! This assumes that bash is available on the remote host

example:

```shell
./makedebremote.sh -O /tmp/debraspbian raspbian hop
```

