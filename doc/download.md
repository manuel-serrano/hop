${ var doc = require( "hopdoc" ) }
${ var xml = require( doc.ROOT + "/doc/xml.js" ) }
${ var cfg = require( doc.ROOT + "/doc/doc.json" ) }

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
       class="danger"
       title="Stable"
       icon="glyphicon-cloud-download"
       href=${cfg.urlbase + "/hop-" + cfg.version + ".tar.gz"}/>
  </div>
</div>}

## Git ##

Hop.js can be forked at

    ${cfg.github}


## Binary distributions ##

Precompiled Hop.js distributions are availabe.

### Debian/Raspberry ###

A Debian/Raspberry repository is available. To use it, add the following
to your `apt` path:

     deb ftp://ftp-sop.inria.fr/indes/fp/Hop/debian squeeze hop
