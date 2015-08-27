/*=====================================================================*/
/*    .../prgm/project/hop/3.0.x/examples/pdfviewer/pdfviewer.js       */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Mon Aug 24 11:28:33 2015                          */
/*    Last change :  Mon Aug 24 20:10:06 2015 (serrano)                */
/*    Copyright   :  2015 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    An example of third-party client-side JavaScript code.           */
/*    -------------------------------------------------------------    */
/*    run: hop -v -g pdfviewer.js                                      */
/*    browser: http://localhost:8080/hop/pdfviewer                     */
/*=====================================================================*/
var hop = require( "hop" );


service pdfviewer( { path: pdfviewer.resource( "example.pdf" ) } ) {
   var ca = <canvas style="width: 200px; height: 200px; border: 1px solid #ccc"/>;
   
   return <html>
     <head jscript=${ [pdfviewer.resource( "pdfwrap.js" ),
		       pdfviewer.resource( "pdf.js" )] }/>
     <body>
       <div>${path}</div>
       ${ ca }
       ~{
	  PDFJS.workerSrc = ${pdfviewer.resource( "pdf.js" )};
	  window.onload = function() {
	     loadPDF( ${path}, function( pdf ) { viewPDF( pdf, 1, ${ca} ) } );
	  }
       }
     </body>
   </html>
}
       
console.log( "Go to \"http://%s:%d/hop/pdfviewer\"", hop.hostname, hop.port );
