<!-- ${ var doc = require( "hopdoc" );
        var config = require( hop.config );
        var xml = require( doc.BUILDDIR + "/doc/xml.js" );
        var cfg = require( doc.BUILDDIR + "/doc/doc.json" ) } -->

Getting HipHop.js
=================

NPM Installation
----------------

```shell
npm install https://www-sop.inria.fr/members/Manuel.Serrano/software/npmx/hop.tgz
```

When Hop is officially released, the installation procedure is:

```shell
npm install @hop/hop
```

Binary Distributions
--------------------

Precompiled Hop.js distributions are available.

### Debian/Raspberry ###

A repository is available. To use it, add the following
to your `apt` path:

```shell
echo 'deb http://hop.inria.fr/linux/Debian bullseye hop' | sudo tee /etc/apt/sources.list.d/hop.list
curl http://www-sop.inria.fr/members/Manuel.Serrano/serrano.pkey | gpg --dearmor | sudo tee /etc/apt/trusted.gpg.d/hop.gpg > /dev/null
sudo apt update
sudo apt install hop
```

The Debian packages are all installed in the `/opt/hop` directory so you will
probably need to modify the `PATH` variable accordingly:

```shell
export PATH=/opt/hop/bin:$PATH
```

### Ubuntu ###

An Ubuntu repository is available. To use it, add the following
to your `apt` path:

```shell
echo 'deb http://hop.inria.fr/linux/Ubuntu jammy hop' | sudo tee /etc/apt/sources.list.d/hop
curl http://www-sop.inria.fr/members/Manuel.Serrano/serrano.pkey | gpg --dearmor | sudo tee /etc/apt/trusted.gpg.d/hop.gpg
```

The Ubuntu packages are all installed in the `/opt/hop` directory. See below.

### Homebrew ###

Homebrew users (MacOS X), can use the pre-built version by using:

```shell
$ brew tap homebrew/hop https://gitlab.inria.fr/mserrano/hopbrew.git
$ brew install homebrew/hop/bigloo-latest
```

## Source code installation ##

${<div class="row">
  <div class="col-xs-8">
This is the file you should download if you want to build the Hop stable
version from the sources.
  </div>
  <div class="col-xs-4">
    <xml.downloadButton
       class="warning"
       title="Stable"
       icon="glyphicon-download"
       href=${cfg.urlbase + "/hop-" + cfg.version + ".tar.gz"}/>
  </div>
</div>}

${<div class="row">
  <div class="col-xs-8">
Compiling Hop.js requires the Bigloo compiler.
  </div>
  <div class="col-xs-4">
    <xml.downloadButton
       class="danger"
       title="Stable"
       icon="glyphicon-download"
       href=${cfg.bglurlbase + "/bigloo-" + cfg.bglversion + ".tar.gz"}/>
  </div>
</div>}

#### Bigloo installation ####

To configure and install the Bigloo compiler that is needed to compile
Hop, use the following:

```shell
$ ./configure --abort-missing && make && sudo make install
```

#### Hop installation ####

To configure and install Hop, execute the following:

```shell
$ ./configure && make && sudo make install
```

Optionally, to compile and install the documentation:

```shell
$ make doc && sudo make install
```

To test the installation:

```shell
$ make test
```


## Complete Installation Guide ##


${doc.include( "../INSTALL.md" )}
