/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/doc/doc.js                        */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Thu Jul 30 17:20:13 2015                          */
/*    Last change :  Fri Jul 31 15:50:43 2015 (serrano)                */
/*    Copyright   :  2015 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    Simple driver to build the documentation.                        */
/*=====================================================================*/
var hop = require( "hop" );
var doc = require( "hopdoc" );
var markdown = require( "markdown" );
var path = require( "path" );

/*---------------------------------------------------------------------*/
/*    compile ...                                                      */
/*---------------------------------------------------------------------*/
function compile( page ) {
   var ast = doc.parseFile( path.join( path.dirname( module.filename ), page ) )
   var toc = doc.toc( ast, "h3" );

   var document = <html>
     <head css=${doc.css} css=${markdown.css}
           jscript=${doc.jscript} rts=${false}/>

     <body data-spy="scroll" data-target="#navbar">
       ~{ $('body').scrollspy( { target: '#navbar' }) }
       <div class="container">
          ${<doc.TITLE>${page.replace( /[.].*$/, "" )}</doc.TITLE>}
          <div class="row">
             <div class="col-md-9" role="main">
                <h1>Table of Contents</h1>
                  <ul>
                    ${toc.map( function( el ) {return <li>${el}</li>} )}
                  </ul>
                ${ast.XML}
             </div>
             <div id="navbar" class="col-md-3" role="complementary">
                <nav class="sidebar"
                 data-spy="affix" data-offset-top="270" data-offset-bottom="20">
                  <ul class="nav bs-docs-sidenav">
                    ${toc.map( function( el ) {
                       return <li role="presentation">${el}</li>;
  	            } )}
                  </ul>
                </nav> 
             </div>
          </div>
       </div>
     </body>
   </html>;

   console.log( hop.XMLCompile( document ) );
}

compile( process.argv[ 2 ] );


