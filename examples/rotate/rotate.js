"use strict";

import { QRCODE } from "./qrcode.js";

service rotate() {
   const url = `http://${this.host}:${hop.port}${followme().toString()}`;
   
   return <html>
     <head>
       <script type="module" lang="hopscript"
	       src=${require.resolve( "./qrcode.js" )}></script>
       <link rel="stylesheet" href=${require.resolve( "./rotate.hss" )}/>
     </head>
     <script>
       server.addEventListener( "orientation", e => {
	  document.getElementById( "container" )
	     .style
	     .setProperty( "transform", `rotate(${360-e.value}deg)`, "" );
       } )
     </script>
     <div id="container" class="center">
       <svg:img height="100%" width="auto" src=${require.resolve( "./phone.svgz" )}/>
     </div>
     <div>
       <qrcode data=${url}/>
       <div><tt>${url}</tt></div>
     </div>
   </html>;
}

service followme() {
   return <html>
     <head>
       <meta name="viewport"
	     content="width=device-width, height=device-height, initial-scale=1, maximum-scale=1, user-scalable=no"/>
       <link rel="stylesheet" href=${require.resolve( "./rotate.hss" )}/>
     </head>
     <script>
       let ot = -1;

       function follow( e ) {
	  if( Date.now() - ot > 150 ) {
	     ot = Date.now();
	     document.getElementById( "log" ).innerHTML = e.gamma;
	     ${orientation}( -e.gamma ).post();
	  } 
       }

       function toggleFollowMe() {
	  if( ot < 0 ) {
	     ot = 0;
	     document.body.setAttribute( "data-following", "on" );
	     window.addEventListener( "deviceorientation", follow );
	  } else {
	     ot = -1;
	     document.body.setAttribute( "data-following", "off" );
	     window.removeEventListener( "deviceorientation", follow );
	     follow( 0 );
	  }
       }
     </script>
     <div id="log"/>
     <div class="center" onclick=~{toggleFollowMe()}>
       <svg:img width="80%" src=${require.resolve( "./phone.svgz" )}/>
     </div>
   </html>
}

service orientation( angle ) {
   hop.broadcast( "orientation", angle );
}
