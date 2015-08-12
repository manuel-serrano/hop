/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/doc/doc.js                        */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Thu Jul 30 17:20:13 2015                          */
/*    Last change :  Wed Aug 12 19:12:59 2015 (serrano)                */
/*    Copyright   :  2015 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    Tools to build the Hop.js documentation.                         */
/*=====================================================================*/
"use hopscript";

const hop = require( "hop" );
const path = require( "path" );
const fs = require( "fs" );
const doc = require( "hopdoc" );
const markdown = require( "markdown" );
const fontifier = require( "fontifier" );
const docxml = require( "./xml.js" );

const ROOT = path.dirname( module.filename );

const css = [ P( "hss/doc.hss" ),
	      P( "lib/bootstrap/css/bootstrap.min.css" ) ];
const jscript = [ P( "lib//jquery/js/jquery.min.js" ),
		  P( "lib/bootstrap/js/bootstrap.min.js" ) ];

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
/*    makeToc ...                                                      */
/*---------------------------------------------------------------------*/
function makeToc( els, k, proc ) {
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
	    var children = makeToc( els, i, proc );
	    acc.push( <ul>${children}</ul> );
	    i += children.length;
	 } else {
	    return acc;
	 }
      }

      return acc;
   }
}

/*---------------------------------------------------------------------*/
/*    compileSection ...                                               */
/*---------------------------------------------------------------------*/
function compileSection( page ) {
   var ast = doc.parseFile( path.join( path.dirname( module.filename ), page ) )
   var toc = doc.toc( ast );
   var title = path.basename( page ).replace( /[0-9]+[-]|[.][^.]*$/g, "" );
   var key = path.basename( title ).toLowerCase();

   var document = <html>
     <head css=${[ fontifier.css, markdown.css, css ]}
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
           <h1 class="toc">Table of Contents</h1>
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
		return el.childNodes[ 0 ].replace( /[(].*$/, "");
	     } )}
                </ul>
             </nav> 
             </div>
             : undefined}
         </div>
       </div>
       <docxml.footer root=${ROOT}/>
     </body>
   </html>;

   console.log( hop.XMLCompile( document ) );
}

/*---------------------------------------------------------------------*/
/*    compileChapter ...                                               */
/*---------------------------------------------------------------------*/
function compileChapter( json ) {
   var chapter = require( json );
   var toc = chapterEntries( chapter );

   var document = <html>
     <head css=${[ fontifier.css, markdown.css, css ]}
           jscript=${jscript}
           rts=${false}/>

     <body data-spy="scroll" data-target="#navbar">
       <docxml.navbar title=${chapter.title}
                        key=${chapter.title.toLowerCase()}>
         ${chapters}
       </docxml.navbar>
       <docxml.title root=${ROOT}>${chapter.title}</docxml.title>

       <div class="container">
         ${chapter.description ? <div class="page-header">
	   ${ fs.existsSync( ROOT + chapter.description ) ?
	      doc.parseFile( ROOT + chapter.description ).XML
	      : markdown.parse( chapter.description ).XML }
	   </div> : ""}
	 
          <h1 class="toc">Table of Contents</h1>
          <ul class="toc">
           ${toc.map( function( el ) {
                        return <li>
	                    <a href=${el.href}>${el.title}</a>
                            <span class="toc-description">
                              ${markdown.parse( el.description )}
                            </span>
	                  </li>
	              } )}
         </ul>
       </div>
       <docxml.footer root=${ROOT}/>
     </body>
   </html>;

   console.log( hop.XMLCompile( document ) );
}

/*---------------------------------------------------------------------*/
/*    compileIndex ...                                                 */
/*---------------------------------------------------------------------*/
function compileIndex( content ) {
   var document = <html>
     <head css=${[ fontifier.css, markdown.css, css ]}
           jscript=${jscript}
           rts=${false}/>

     <body class="home" data-spy="scroll" data-target="#navbar">
       <docxml.navbar title="Hop.js" key="Home">
         ${chapters}
       </docxml.navbar>
       <docxml.title root=${ROOT}>Hop.js</docxml.title>

       <div class="container">
         ${doc.parseFile( path.join( path.dirname( module.filename ), "hop.md" ) ).XML}
       </div>
       <docxml.footer root=${ROOT}/>
     </body>
   </html>;

   console.log( hop.XMLCompile( document ) );
}

/*---------------------------------------------------------------------*/
/*    top level forms                                                  */
/*---------------------------------------------------------------------*/
if( process.argv[ 2 ].match( /[.]md$/ ) ) {
   if( process.argv[ 2 ] === "hop.md" ) {
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
