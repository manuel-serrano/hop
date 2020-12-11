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
"use hopscript"

const fs = require( 'fs' );
const path = require( 'path' );

import * as sp from hop.spage;

import { NAVTITLE } from './xml.js';
import { APPS } from './apps.js';
import { SYSTEM } from './system.js';
import { WEBDAV } from './webdav.js';
import { ABOUT } from './about.js';
import { init as configInit } from './config.js';

/*---------------------------------------------------------------------*/
/*    Initialize the hopdroid config system.                           */
/*---------------------------------------------------------------------*/
configInit();
   
/*---------------------------------------------------------------------*/
/*    hopdroid                                                         */
/*---------------------------------------------------------------------*/
service hopdroid( o ) {
   return <html>
     
     <head>
       <meta name="viewport" 
	     content="width=device-width, height=device-height, initial-scale=1, maximum-scale=1, user-scalable=no"/>
       <script src=${sp.script} type="application/x-javascript"/>
       <link rel="manifest" href=${manifest()}/>
       <link href=${sp.css} rel="stylesheet" type="text/css"/>
       <link href=${require.resolve( "./hopdroid.hss" )} rel="stylesheet" type="text/css"/>
       <link href=${require.resolve( "./xml.hss" )} rel="stylesheet" type="text/css"/>
       <link href=${require.resolve( "./apps.hss" )} rel="stylesheet" type="text/css"/>
       <link href=${require.resolve( "./webdav.hss" )} rel="stylesheet" type="text/css"/>
       <link href=${require.resolve( "./system.hss" )} rel="stylesheet" type="text/css"/>
       <link href=${require.resolve( "./about.hss" )} rel="stylesheet" type="text/css"/>
       <script>
	 hop.addEventListener( "configurationchange", e => console.log( "config: ", e ) );
       </script>
     </head>
     
     <body class="hopdroid">
       <sp.spage id="spage">
	 <sp.sphead class="main">
	   <navtitle spageid="spage" class="selected">Hop</navtitle>
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

function PRIVACY( attrs ) {
   return <sp.sptab svc=${service () { return "not-implemented" } }>
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

/*---------------------------------------------------------------------*/
/*    manifest ...                                                     */
/*---------------------------------------------------------------------*/
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

