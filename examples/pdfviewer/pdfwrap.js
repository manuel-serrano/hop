/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/examples/pdfviewer/pdfwrap.js     */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Mon Aug 24 11:31:02 2015                          */
/*    Last change :  Mon Aug 24 11:32:49 2015 (serrano)                */
/*    Copyright   :  2015 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    Binder to the Mozilla PDF viewer.                                */
/*    -------------------------------------------------------------    */
/*    This code intentionally builds a standard XMLHttpRequest to      */
/*    fetch the PDF document.                                          */
/*=====================================================================*/
function loadPDF( src, onload ) {
   var xhr = new XMLHttpRequest();

   xhr.responseType = "arraybuffer";
   xhr.open( "GET", src );

   xhr.onreadystatechange = function() {
      if( xhr.readyState == 4 && xhr.status == 200 ) {
	 var pdf = new PDFJS.PDFDoc( xhr.response );
	 if( onload ) onload( pdf );
      }
   }
   
   xhr.send();
}

function viewPDF( pdf, pagenum, canvas ) {
   var page = pdf.getPage( pagenum );
   
   canvas.height = page.height;
   canvas.width = page.width;
   
   page.startRendering( canvas.getContext( "2d" ) );
}
   
