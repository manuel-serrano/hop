${ var doc = require( "hopdoc" ) }
${ var config = require( hop.config ) }
${ var xml = require( doc.BUILDDIR + "/doc/xml.js" ) }
${ var cfg = require( doc.BUILDDIR + "/doc/doc.json" ) }
${ var bibtex = require( "./_bibtex.hop" ) }

Citations
---------

To cite the Hop software, please use the following Biblatex entry.

${<pre class="bibtex">
@software{ hop,
   title = {Hop, multitier Web Programming},
   author = {Serrano, Manuel},
   year = {2006},
   institution = {Inria},
   url = {${cfg.homepage}}
}
</pre>}

For referring to the current release, please use:

${<pre class="bibtex">
@softwareversion{ hop-${cfg.version},
  version = {${cfg.version}},
  year = {${cfg.date.split( " " )[ 2 ]}},
  month = {${cfg.date.split( " " )[ 1 ]}},
  file = {${cfg.urlbase}/hop-${cfg.version}},
  crossref = {hop}
}
</pre>}


References
----------

${ function suffix( e, path ) {
if( !path ) console.error( e );
    if( path.match( /[.]ps[.]gz$/ ) ) {
       return "ps.gz";
    } else if( path.lastIndexOf( "." ) > 0 ) {
       return path.substring( path.lastIndexOf( "." ) + 1 );
    } else {
       return path;
    }
  }
}

${ bibtex.load( "./hop.bib" )
  .sort( (x, y) => x.year < y.year ? true : x.year > y.year ? false : x.month < y.month )
  .map( e => 
<div class="bibentry">
  <span class="author">${e.author}</span>
  <span class="title">${e.title}</span>
  <span class="booktitle">${e.booktitle || e.journal}</span>,
  <span class="address">${e.address}</span>,
  <span class="month">${e.month}</span>,
  <span class="year">${e.year}</span>
  <div class="download">
     <a href=${e.download}>${suffix( e, e.download || e.url )}</a>
  </div>
  <div class="abstract">
    ${e.abstract}
  </div>
</div> ) }

