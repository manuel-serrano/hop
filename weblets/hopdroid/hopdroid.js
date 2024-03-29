/*=====================================================================*/
/*    serrano/prgm/project/hop/hop/weblets/hopdroid/hopdroid.js        */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Wed Nov 25 07:08:20 2020                          */
/*    Last change :  Sun Jan 23 07:39:14 2022 (serrano)                */
/*    Copyright   :  2020-22 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Hopdroid application                                             */
/*=====================================================================*/
"use hopscript"

import fs from 'fs';
import path from 'path';

import * as sp from hop.spage;

import { NAVTITLE } from './xml.js';
import { APPS } from './apps.js';
import { SYSTEM } from './system.js';
import { PRIVACY } from './privacy.js';
import { WEBDAV } from './webdav.js';
import { ABOUT } from './about.js';
import * as localConfig from './config.js';
import { phone } from './phone.js';
import { authorizeRequest } from './auth.js';

/*---------------------------------------------------------------------*/
/*    Initialize the hopdroid config system.                           */
/*---------------------------------------------------------------------*/
localConfig.init();
   
/*---------------------------------------------------------------------*/
/*    hopdroidSVC ...                                                  */
/*---------------------------------------------------------------------*/
service hopdroidSVC() {
   return authorizeRequest( this, hopdroid );
}

/*---------------------------------------------------------------------*/
/*    hopdroid ...                                                     */
/*---------------------------------------------------------------------*/
function hopdroid( req ) {
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
       <link href=${require.resolve( "./privacy.hss" )} rel="stylesheet" type="text/css"/>
       <link href=${require.resolve( "./webdav.hss" )} rel="stylesheet" type="text/css"/>
       <link href=${require.resolve( "./system.hss" )} rel="stylesheet" type="text/css"/>
       <link href=${require.resolve( "./about.hss" )} rel="stylesheet" type="text/css"/>
       <script>
	 server.addEventListener( "configurationchange", e => console.log( "config: ", e ) );
       </script>
     </head>
     
     <body class="hopdroid"
	   data-theme=${`${localConfig.theme || (hop.isLocalRequest( req ) && phone.config.theme) || "default"}`}>
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

/*---------------------------------------------------------------------*/
/*    services                                                         */
/*---------------------------------------------------------------------*/
hopdroidSVC.path = "/hop/hopdroid";
