${ var doc = require( "hopdoc" ) }
${ var xml = require( doc.ROOT + "/doc/xml.js" ) }
${ var cfg = require( doc.ROOT + "/doc/doc.json" ) }

### License ###

${doc.include( "./license.md" )}

## Source code ##

### Stable ###

${<div class="row">
  <div class="col-xs-9">
This is the file you should download if you have Bigloo ${cfg.bglversion} or
more recent already installed.
  </div>
  <div class="col-xs-3">
    <xml.downloadButton
       class="danger"
       title="Stable"
       icon="glyphicon-cloud-download"
       href=${cfg.urlbase + "/hop-" + cfg.version + ".tar.gz"}/>
  </div>
</div>}

#### Requirements  ####

 1. **bigloo-${cfg.bglversion}**
 2. libunistring
 3. openssl
 4. autoconf
 5. automake
 6. libtool

${<div class="row">
  <div class="col-xs-9">
This is the file you should download if you don't have Bigloo ${cfg.bglversion} 
but you have an up-to-date development environment installed on your machine.
  </div>
  <div class="col-xs-3">
    <xml.downloadButton
       class="warning"
       title="Stable"
       icon="glyphicon-cloud-download"
       href=${cfg.urlbase + "/hop-bigloo-" + cfg.version + ".tar.gz"}/>
  </div>
</div>}

#### Requirements  ####

 1. libunistring
 2. openssl
 3. autoconf
 4. automake
 5. libtool
 
${<div class="row">
  <div class="col-xs-9">
This is the file you should download if you don't have development 
libraries already installed on your machine. This version only requires
`autoconf`, `automake`, and `libtool` installed. 
  </div>
  <div class="col-xs-3">
    <xml.downloadButton
       class="success"
       title="Stable"
       icon="glyphicon-cloud-download"
       href=${cfg.urlbase + "/hop-full-" + cfg.version + ".tar.gz"}/>
  </div>
</div>}

#### Requirements  ####

 1. autoconf
 2. automake
 3. libtool

## Git ##

Hop.js can be forked at

    ${cfg.github}


## Binary distributions ##

Precompiled Hop.js distributions are availabe.

### Debian/Raspberry ###

A Debian/Raspberry repository is available. To use it, add the following
to your `apt` path:

     deb ftp://ftp-sop.inria.fr/indes/fp/Hop/debian squeeze hop
