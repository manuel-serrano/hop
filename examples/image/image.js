/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/examples/image/image.js           */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Apr 18 09:41:10 2014                          */
/*    Last change :  Thu Aug 20 07:14:41 2015 (serrano)                */
/*    Copyright   :  2014-15 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Image manipulation example                                       */
/*    -------------------------------------------------------------    */
/*    run: hop -v image.js                                             */
/*    browser: http://localhost:8080/hop/image                         */
/*=====================================================================*/
var img_default = "http://t1.gstatic.com/images?q=tbn:ANd9GcRUADxj_7NEl8RAFNM-s6x3Wgp1QIg81QHRMVeOuMHBklr1JWddmQ";

var colors_default = [
   {red: 252, green: 27, blue: 0},
   {red: 125, green: 167, blue: 129},
   {red: 255, green: 122, blue: 10}
];

service imgProxy( url ) {
   return hop.HTTPResponseProxy( url );
}

service image( o ) {
   var url = o && "url" in o ? o.url : img_default;
   
   return <html>
     ~{
	function drawImage( url, colors ) {
	   var img = new Image();
	   var src = document.getElementById( "src" );
	   var dst = document.getElementById( "dst" );
	   var ctxsrc = src.getContext( "2d" );
	   var ctxdst = dst.getContext( "2d" );

	   img.onload = function( e ) {
	      src.width = img.width;
	      src.height = img.height;
	      dst.width = img.width;
	      dst.height = img.height;

	      ctxsrc.drawImage( img, 0, 0 );
	      var f = ctxsrc.getImageData( 0, 0, img.width, img.height );
	      ctxdst.putImageData( colorize( f, colors ), 0, 0 );
	   }

	   img.src = url;
	}

	function colorize( frame, colorset ) {
	   var data = frame.data;
	   var l = data.length / 4;

	   for( var i = 0; i < l; i++ ) {
	      var r = data[ i*4 ];
	      var g = data[ i*4 + 1 ];
	      var b = data[ i*4 + 2 ];
	      var range = Math.floor( (0.65*r + 0.22*g + 0.13*b) / 86 );

	      var cs = colorset[ range ];
	      
	      data[ i*4 ] = 0.7*cs.red + 0.3*r;
	      data[ i*4 + 1 ] = 0.7*cs.green + 0.3*g;
	      data[ i*4 + 2 ] = 0.7*cs.blue + 0.3*b;
	   }

	   return frame;
	}
	
	window.addEventListener( "load", function( e ) {
	   drawImage( ${imgProxy( url )}, ${colors_default} )
	} )
     }
     <img src=${url}/>
     <canvas id="src" style="display: none"/>
     <canvas id="dst"/>
   </html>
}
   
console.log( "Go to \"http://%s:%d/hop/image\"", hop.hostname, hop.port );
