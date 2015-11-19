${ var doc = require( "hopdoc" ) }
${ var config = require( hop.config ) }
${ console.error( "DOC.build=", doc.BUILDDIR ) }
${ var xml = require( doc.BUILDDIR + "/doc/xml.js" ) }
${ var cfg = require( doc.BUILDDIR + "/doc/doc.json" ) }

## License ##

${doc.include( "./license.md" )}

## Source code ##

${<div class="row">
  <div class="col-xs-9">
This is the file you should download if you want to build the Hop stable
version from the sources.
  </div>
  <div class="col-xs-3">
    <xml.downloadButton
       class="warning"
       title="Stable"
       icon="glyphicon-download"
       href=${cfg.urlbase + "/hop-" + cfg.version + ".tar.gz"}/>
  </div>
</div>}

${<div class="row">
  <div class="col-xs-9">
Compiling Hop.js requires the Bigloo compiler.
  </div>
  <div class="col-xs-3">
    <xml.downloadButton
       class="danger"
       title="Stable"
       icon="glyphicon-download"
       href=${cfg.bglurlbase + "/bigloo" + cfg.bglversion + ".tar.gz"}/>
  </div>
</div>}

#### Bigloo installation ####

To configure and install the Bigloo compiler that is needed to compile
Hop, use the following:

```shell
./configure --abort-missing && make && sudo make install
```

#### Hop installation ####

To configure and install Hop, execute the following:

```shell
./configure && make && sudo make install
```

Optionally, to compile and install the documentation:

```shell
make doc && sudo make install
```

To test the installation:

```shell
make test
```

## Binary distributions ##

Precompiled Hop.js distributions are available.

### Debian/Raspberry ###

A Debian/Raspberry repository is available. To use it, add the following
to your `apt` path:

     deb ftp://ftp-sop.inria.fr/indes/fp/Hop/debian squeeze hop


## Git ##

Hop.js can be forked at

${<a href=${cfg.github}>${cfg.github}</a>}


## Older releases ##

Old Hop releases can be obtain at the following address:

<ftp://ftp-sop.inria.fr/indes/fp/Hop/>
