/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/doc/doc.js                        */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Thu Jul 30 17:20:13 2015                          */
/*    Last change :  Mon Aug  3 14:27:49 2015 (serrano)                */
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
   return path.normalize( path.join( ROOT, file ) );
}
   
/*---------------------------------------------------------------------*/
/*    chapters ...                                                     */
/*---------------------------------------------------------------------*/
const chapters = require( "./doc.json" ).chapters
      .map( function( c ) { c.entries = chapterEntries( c ); return c; } );

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
/*    compileSection ...                                               */
/*---------------------------------------------------------------------*/
function compileSection( page ) {
   var ast = doc.parseFile( path.join( path.dirname( module.filename ), page ) )
   var toc = doc.toc( ast, "h3" );
   var title = path.basename( page ).replace( /[0-9]+[-]|[.][^.]*$/g, "" );
   var key = path.dirname( title ).toLowerCase();

   var document = <html>
     <head css=${[ fontifier.css, markdown.css, css ]}
           jscript=${jscript}
           rts=${false}/>

     <body data-spy="scroll" data-target="#navbar">
       ~{ $('body').scrollspy( { target: '#navbar' }) }
       <div class="container">
          <docxml.title root=${ROOT}>${title}</docxml.title>
          <div class="row">
             <div class=${toc == [] ? "col-md-12" : "col-md-9"} role="main">
   
               <docxml.navchapters title=${title} key=${key}>
                 ${chapters}
                </docxml.navchapters>
   
                <h1>Table of Contents</h1>
                  <ul class="toc">
                    ${toc.map( function( el ) {return <li>${el}</li>} )}
                  </ul>
                ${ast.XML}
             </div>

             ${(toc.length > 0) ?
                <div id="navbar" class="col-md-3" role="complementary">
                   <nav class="sidebar"
                        data-spy="affix"
	                data-offset-top="270" data-offset-bottom="20">
                     <ul class="nav bs-docs-sidenav">
                       ${toc.map( function( el ) {
                          return <li role="presentation">
                            <a href=${el.href}>
                              ${el.childNodes[ 0 ].replace( /[(].*$/, "")}
                            </a>
                          </li>;
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

   chapters.forEach( function( c ) { c.entries = chapterEntries( chapter ) } );
   
   var document = <html>
     <head css=${[ fontifier.css, markdown.css, css ]}
           jscript=${jscript}
           rts=${false}/>

     <body data-spy="scroll" data-target="#navbar">
       <div class="container">
         <docxml.title root=${ROOT}>${chapter.title}</docxml.title>
         <docxml.navchapters title=${chapter.title}
                             key=${chapter.title.toLowerCase()}>
           ${chapters}
         </docxml.navchapters>
   
          <h1>Table of Contents</h1>
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

     <body data-spy="scroll" data-target="#navbar">
       <div class="container">
         <docxml.title root=${ROOT}>Hop.js</docxml.title>
         <docxml.navchapters title="Hop.js" key="">
           ${chapters}
         </docxml.navchapters>

         ${require( content )}
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
   compileSection( process.argv[ 2 ] );
} else if( process.argv[ 2 ].match( /[.]json$/ ) ) {
   compileChapter( "./" + process.argv[ 2 ] );
} else if( process.argv[ 2 ].match( /[.]html$/ ) ) {
   compileIndex( "./" + process.argv[ 2 ] );
} else if( fs.lstatSync( process.argv[ 2 ] ).isDirectory() ) {
   compileChapter( { title: path.basename( process.argv[ 2 ] ),
		     files: [ process.argv[ 2 ] ] } )
}
      


