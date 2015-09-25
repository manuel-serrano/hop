/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/doc/doc.js                        */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Thu Jul 30 17:20:13 2015                          */
/*    Last change :  Fri Sep 25 09:24:53 2015 (serrano)                */
/*    Copyright   :  2015 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    Tools to build the Hop.js documentation.                         */
/*=====================================================================*/
"use hopscript";

/*---------------------------------------------------------------------*/
/*    module imports                                                   */
/*---------------------------------------------------------------------*/
const hop = require( "hop" );
const path = require( "path" );
const fs = require( "fs" );
const markdown = require( hop.markdown );
const fontifier = require( hop.fontifier );
const doc = require( "hopdoc" );
const docxml = require( "./xml.js" );

/*---------------------------------------------------------------------*/
/*    global parameters                                                */
/*---------------------------------------------------------------------*/
const ROOT = path.dirname( module.filename );

const css = [ P( "hss/doc.hss" ),
	      P( "lib/bootstrap/css/bootstrap.min.css" ) ];
const jscript = [ P( "lib//jquery/js/jquery.min.js" ),
		  P( "lib/bootstrap/js/bootstrap.min.js" ) ];

const alias = {
   "user": "api",
   "config": "api",
   "hss": "api",
   "markdown": "api",
   "tree": "widget",
   "spage": "widget"
}

/*---------------------------------------------------------------------*/
/*    P ...                                                            */
/*---------------------------------------------------------------------*/
function P( file ) {
   return path.normalize( "./" + file );
}
   
/*---------------------------------------------------------------------*/
/*    chapters ...                                                     */
/*---------------------------------------------------------------------*/
const chapters = require( "./doc.json" )
      .chapters
      .map( function( c ) {
	 c.entries = chapterEntries( c );
	 return c;
      } );

/*---------------------------------------------------------------------*/
/*    chapterEntries ...                                               */
/*---------------------------------------------------------------------*/
function chapterEntries( chapter ) {
   
   function chapterFile( file ) {
      var base = path.basename( file );
      return {
	 path: file.replace( /[.]md$/, ".html" ),
	 href: base.replace( /[.]md$/, ".html" ),
	 title: base.replace( /[0-9]+[-]|[.]md$/g, "" )
      };
   }
   
   function chapterEntry( file ) {
      var fp = path.join( ROOT, file );
      if( fs.lstatSync( fp ).isDirectory() ) {
	 return fs.readdirSync( fp )
	    .filter( function( e ) {
	       return e.match( /[.]md$/ ) && (e != "index.md");
	    } )
	    .sort( function( left, right ) {
	       return left.naturalCompare( right );
	    } )
	    .map( chapterFile );
      } else {
	 return [ chapterFile( file ) ];
      }
   }

   if( chapter.json ) {
      var c = require( "./" + chapter.json );
      return Array.prototype.concat.apply( [], c.files.map( chapterEntry ) );
   } else if( chapter.files ) {
      return Array.prototype.concat.apply( [], chapter.files.map( chapterEntry ) );
   } else {
      return [];
   }
}

/*---------------------------------------------------------------------*/
/*    childrenSize ...                                                 */
/*---------------------------------------------------------------------*/
function childrenSize( children ) {
   var res = 0;
   
   for( var i = 0; i < children.length; i++ ) {
      if( children[ i ].tagName == "ul" ) {
	 res += childrenSize( children[ i ].childNodes );
      } else if( children[ i ].tagName == "li" ) {
	 res++;
      }
   }
   return res;
}

/*---------------------------------------------------------------------*/
/*    makeToc ...                                                      */
/*---------------------------------------------------------------------*/
function _makeToc( els, k, proc, indent ) {
   if( els.length == k  ) {
      return [];
   } else {
      var acc = [];
      var tag = els[ k ].tagName;

      for( var i = k; i < els.length; ) {
	 if( els[ i ].tagName == tag ) {
	    var el = els[ i++ ];
	    var n = proc ? proc( el ) : el.childNodes;
	    acc.push( <li>
	      <a href=${"#" + el.id} role="presentation">
		${n}</a></li> );
	 } else if( els[ i ].tagName > tag ) {
	    var children = _makeToc( els, i, proc, indent + "  " );
	    acc.push( <ul>${children}</ul> );
	    i += childrenSize( children );
	 } else {
	    return acc;
	 }
      }

      return acc;
   }
}

function makeToc( els, k, proc ) {
   return _makeToc( els, k, proc, "" );
}

/*---------------------------------------------------------------------*/
/*    compileSection ...                                               */
/*---------------------------------------------------------------------*/
function compileSection( page ) {
   var ast = doc.load( path.join( path.dirname( module.filename ), page ) )
   var toc = doc.toc( ast );
   var title = path.basename( page ).replace( /[0-9]+[-]|[.][^.]*$/g, "" );
   var chapter = path.basename( path.dirname( path ) );
   var key = path.basename( path.dirname( page ) ).toLowerCase();
   
   if( key == "doc" ) {
      key = alias[ path.basename( page ) ];
   } else if( key == "." ) {
      key = title;
   }
   
   var document = <html>
     <head css=${[ fontifier.css, markdown.css, css ]}
	   title=${title}
           jscript=${jscript}
           rts=${false}/>

     <body data-spy="scroll" data-target="#navbar" class=${title}>
       ~{ $('body').scrollspy( { target: '#navbar' }) }
       <docxml.navbar title=${title} key=${key}>
         ${chapters}
       </docxml.navbar>
       
       <docxml.title root=${ROOT}>${title}</docxml.title>
       <div class="container">
         <div class=${toc == [] ? "col-md-12" : "col-md-9"} role="main">
           <h1 class="toc" id="toc">Table of Contents</h1>
           <ul class="toc">
             ${makeToc( toc, 0 )}
           </ul>
           ${ast.XML}
         </div>
         <div class="row">
           ${(toc.length > 0) ?
           <div id="navbar" class="col-md-3" role="complementary">
             <nav class="sidebar"
		  data-spy="affix"
	          data-offset-top="215" data-offset-bottom="100">
               <ul class="nav bs-docs-sidenav">
                  ${makeToc( toc, 0, function( el ) {
		     return el.childNodes[ 0 ].data.replace( /[(].*$/, "");
		  } )}
	       </ul>
	     </nav> 
	   </div>
           : undefined}
	 </div>
	 <docxml.footer root=${ROOT}/>
       </div>
     </body>
   </html>;

   console.log( hop.xmlCompile( document ) );
}

/*---------------------------------------------------------------------*/
/*    compileChapter ...                                               */
/*---------------------------------------------------------------------*/
function compileChapter( json ) {
   var chapter = require( json );
   var toc = chapterEntries( chapter );

   var document = <html>
     <head css=${[ fontifier.css, markdown.css, css ]}
	   title=${chapter.title}
           jscript=${jscript}
           rts=${false}/>

     <body data-spy="scroll" data-target="#navbar">
       <docxml.navbar title=${chapter.title}
                      key=${chapter.key}>
         ${chapters}
       </docxml.navbar>
       <docxml.title root=${ROOT}>${chapter.title}</docxml.title>

       <div class="container">
         ${chapter.description ? <div class="chapter-header">
	   ${ fs.existsSync( ROOT + "/" + chapter.description ) ?
	      doc.load( ROOT + "/" + chapter.description ).XML
	      : doc.eval( chapter.description ).XML }
	   </div> : ""}
	 
         <h1 class="toc" id="toc">Table of Contents</h1>
         <ul class="toc">
           ${toc.map( function( el ) {
              return <li>
	        <a href=${el.href}>${el.title}</a>
                <span class="toc-description">
                  ${doc.eval( el.description )}
                </span>
	      </li>
	   } )}
         </ul>
	 <docxml.footer root=${ROOT}/>
       </div>
     </body>
   </html>;

   console.log( hop.xmlCompile( document ) );
}

/*---------------------------------------------------------------------*/
/*    compileIndex ...                                                 */
/*---------------------------------------------------------------------*/
function compileIndex( content ) {
   var document = <html>
     <head css=${[ fontifier.css, markdown.css, css ]}
	   title="Hop.js"
           jscript=${jscript}
           rts=${false}/>

     <body class="home" data-spy="scroll" data-target="#navbar">
       <docxml.navbar title="Hop.js" key="home">
         ${chapters}
       </docxml.navbar>
       <docxml.title root=${ROOT}/>

       <div class="container">
         ${doc.load( path.join( path.dirname( module.filename ), "_index.md" ) ).XML}
	 <docxml.footer root=${ROOT}/>
       </div>
     </body>
   </html>;

   console.log( hop.xmlCompile( document ) );
}

/*---------------------------------------------------------------------*/
/*    bind dummy xml construct                                         */
/*---------------------------------------------------------------------*/
(function( tags ) {
   function ignore( attr, _ ) { return undefined; };

   tags.forEach( function( tag ) { this[ tag ] = ignore } );
})( require( "./xml-ignore.json" ) );

/*---------------------------------------------------------------------*/
/*    mkIdx ...                                                        */
/*---------------------------------------------------------------------*/
function mkIdx( base, files ) {
   var table = [];

   for( i = 0; i < files.length; i++ ) {
      var file = files[ i ];
      var xml = require( "./" + file );
      var chapter = path.basename( file, ".html" ).replace( /^[0-9]+-/, "" );

      var idx = doc.index( { XML: xml } ).map( function( e ) {
	 e.chapter = chapter;
	 e.url = file + "#" + e.id;
	 return e;
      } );

      table = table.concat( idx );
   }
   
   console.log(
      JSON.stringify(
	 table.sort( function( l, r ) {
	    return l.key.localeCompare( r.key ); } ) ) );
}

/*---------------------------------------------------------------------*/
/*    compileIdx ...                                                   */
/*---------------------------------------------------------------------*/
function compileIdx( json ) {
   var idx = require( json );
   var chapter = { title: "Index", key: "index" };

   var document = <html>
     <head css=${[ fontifier.css, markdown.css, css ]}
	   title=${chapter.title}
           jscript=${jscript}
           rts=${false}/>

     <body data-spy="scroll" data-target="#navbar">
       <docxml.navbar title=${chapter.title}
                      key=${chapter.key}>
         ${chapters}
       </docxml.navbar>
       <docxml.title root=${ROOT}>${chapter.title}</docxml.title>

       <div class="container">
	 <docxml.idx>${idx}</docxml.idx>
	 <docxml.footer root=${ROOT}/>
       </div>
     </body>
   </html>;

   console.log( hop.xmlCompile( document ) );
}

/*---------------------------------------------------------------------*/
/*    top level forms                                                  */
/*---------------------------------------------------------------------*/
if( process.argv[ 2 ] == "mkidx" ) {
   mkIdx( process.argv[ 3 ], process.argv.slice( 4 ) );
} else if( process.argv[ 2 ] == "cmpidx" ) {
   compileIdx( "./" + process.argv[ 3 ] );
} else if( process.argv[ 2 ].match( /[.]md$/ ) ) {
   if( process.argv[ 2 ] === "_index.md" ) {
      compileIndex( "./" + process.argv[ 2 ] );
   } else {
      compileSection( process.argv[ 2 ] );
   }
} else if( process.argv[ 2 ].match( /[.]json$/ ) ) {
   compileChapter( "./" + process.argv[ 2 ] );
} else if( process.argv[ 2 ].match( /[.]html$/ ) ) {
   compileIndex( "./" + process.argv[ 2 ] );
} else if( fs.lstatSync( process.argv[ 2 ] ).isDirectory() ) {
   compileChapter( { title: path.basename( process.argv[ 2 ] ),
		     files: [ process.argv[ 2 ] ] } )
}
