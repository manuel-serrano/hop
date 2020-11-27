/*=====================================================================*/
/*    serrano/prgm/project/hop/work/hopdroid/hopdroid.js               */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Wed Nov 25 07:08:20 2020                          */
/*    Last change :  Thu Nov 26 16:42:55 2020 (serrano)                */
/*    Copyright   :  2020 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    Hopdroid application                                             */
/*=====================================================================*/
var fs = require( 'fs' );
var path = require( 'path' );

import * as sp from hop.spage;

import { NAVTITLE } from './xml.js';
import { APPS } from './apps.js';
import { SYSTEM } from './system.js';
import { ABOUT } from './about.js';

/*---------------------------------------------------------------------*/
/*    hopdroid                                                         */
/*---------------------------------------------------------------------*/
service hopdroid( o ) {
   return <html>
     
     <head>
       <meta name="viewport" 
	     content="width=device-width, height=device-height, initial-scale=1, maximum-scale=1, user-scalable=no"/>
       <link rel="manifest" href=${manifest()}/>
       <link href=${sp.css} rel="stylesheet" type="text/css"/>
       <link href=${require.resolve( "./hopdroid.hss" )} rel="stylesheet" type="text/css"/>
       <link href=${require.resolve( "./apps.hss" )} rel="stylesheet" type="text/css"/>
       <link href=${require.resolve( "./system.hss" )} rel="stylesheet" type="text/css"/>
       <link href=${require.resolve( "./about.hss" )} rel="stylesheet" type="text/css"/>
       <script src=${sp.script} type="application/x-javascript"/>
     </head>
     
     <body class="hopdroid">
       <sp.spage id="spage">
	 <sp.sphead class="main">
	   <navtitle spageid="spage">Hop</navtitle>
	 </sp.sphead>

	 <div class="spage-block">
	   <privacy/>
	 </div>
	 
	 <div class="spage-block">
	   <webdav/>
	   <apps/>
	 </div>

	 <div class="spage-block">
	   <system/>
	   <about/>
	 </div>
	 
	 <div class="spage-block">
	 </div>
	 
       </sp.spage>
     </body>
   </html>;
}

function PRIVACY() {
   return <sp.sptab svc=${webdav}>
     <sp.sptabhead>
       <nav class="sptabhead unselected">
	 <ul>
	   <li>
	     <div class="icon">
	       <svg:img class="privacy-icon" width="16px" height="16px" 
			src=${require.resolve( "./icons/shield-lock.svg" )}/>
	     </div>
	   </li>
	   <li>
	     <div class="title">
	       <div>Security</div>
	       <div class="subtitle">authentication, permissions</div>
	     </div>
	   </li>
	 </ul>
       </nav>
       <navtitle spageid="spage" class="sphead selected" arrow="&#8672;">Privacy</navtitle>
     </sp.sptabhead>
   </sp.sptab>
}

function WEBDAV() {
   return <sp.sptab svc=${webdav}>
     <sp.sptabhead>
       <nav class="sptabhead unselected">
	 <ul>
	   <li>
	     <div class="icon">
	       <svg:img class="hdd-icon" width="16px" height="16px" 
			src=${require.resolve( "./icons/hdd-network.svg" )}/>
	     </div>
	   </li>
	   <li>
	     <div class="title">
	       <div>Webdav configuration</div>
	       <div class="subtitle">hdd sharing</div>
	     </div>
	   </li>
	 </ul>
       </nav>
       <navtitle spageid="spage" class="sphead selected" arrow="&#8672;">Hdd Sharing</navtitle>
     </sp.sptabhead>
   </sp.sptab>
}
   
service webdav() {
   return <div>
     <div class="setting">
       <div class="icon-sans">
	 <svg:img class="about-icon" width="16px" height="16px" 
		  src=${require.resolve( "./icons/hdd.svg" )}/>
       </div>
       <div class="text">
	 Enable webdav
       </div>
       <div class="button"
	    onclick=~{document.getElementById( "webdav-toggle" ).setAttribute( "data-on", "on" )}>
	 <svg:img id="webdav-toggle" class="icon-toggle" width="24px" height="24px" 
		  src=${require.resolve( "./icons/toggle2-off.svg" )}/>
       </div>
     </div>
   </div>;
}
     
service manifest() {
   return hop.HTTPResponseString( 
      `{"short_name": "hopc@${hop.hostname}", 
		  "name": "hop v${hop.version}", 
                  "start_url": "/hop/hopdroid", 
		  "display": "standalone", 
                  "orientation": "portrait",
                  "description": "Hop on Android",
                  "icons": [ { "src": "${require.resolve( './hop.svg' )}", "type": "image/svg" } ]}`,
      { "charset": hop.charset,
	"header": {
	   "Cache-Control": "no-cache",
	   "Pragma": "no-cache"
	},
	"content-type": "application/manifest+json",
	"bodyp": false,
	"content-length": -1
      } );
}

