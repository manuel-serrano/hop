![http://hop.inria.fr](./doc/logo.svg) Hop -- Multitier Web Programming
=======================================================================

<!-- github -->
![branch workflow](https://github.com/manuel-serrano/hop/actions/workflows/hop.yml/badge.svg)
<!-- /github -->

Hop is a multitier environment for Web applications. Hop applications can
be developped in either

  * JavaScript/TypeScript;
  * Scheme.
  
<!-- github -->
The JavaScript documentation is available in two formats:

|     HTML generic documentation     | Markdown commit documentation     |
|------------------------------------|-----------------------------------|
| [HTML](http://hop.inria.fr)        | [markdown](./doc/README.md)       |

The Scheme documentation is available online:

|     HTML generic documentation     |
|------------------------------------|
| [HTML](http://hop.inria.fr/hop/doc?lang=hop)
<!-- /github -->

JavaScript/TypeScript Multitier Programming
-------------------------------------------

```shell
$ npm install https://www-sop.inria.fr/members/Manuel.Serrano/software/npmx/hop.tgz --save
```

Scheme Multitier Programming
----------------------------

Prerequisites:

 * GCC
 * Bigloo 4.5b or newer <http://www-sop.inria.fr/indes/fp/Bigloo>
 * GNU Make 3.81 or newer
 * Autoconf/Automake/Libtool
 * OpenSSL 1.1 or newer

Unix/Machintosh:

```sh
./configure && make && make install
```

### To run the tests:

```sh
make test
```

Resources
---------

 * <http://hop.inria.fr>
