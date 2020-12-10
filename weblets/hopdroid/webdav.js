/*=====================================================================*/
/*    serrano/prgm/project/hop/hop/weblets/hopdroid/webdav.js          */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Wed Dec  2 16:36:06 2020                          */
/*    Last change :  Wed Dec  2 16:36:06 2020 (serrano)                */
/*    Copyright   :  2020 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    webdav configuration                                             */
/*=====================================================================*/
var fs = require( 'fs' );
var path = require( 'path' );

import { hopdroid } from hop.hopdroid;
import * as sp from hop.spage;
import { NAVTITLE } from './xml.js';
import { config, update as updateConfig } from './config.js';

/*---------------------------------------------------------------------*/
/*    WEBDAV ...                                                       */
/*---------------------------------------------------------------------*/
export function WEBDAV() {
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
	       <div>WebDav configuration</div>
	       <div class="subtitle">storage sharing</div>
	     </div>
	   </li>
	 </ul>
       </nav>
       <navtitle spageid="spage" class="sphead selected" arrow="&#8672;">
	 WebDav configuration
       </navtitle>
     </sp.sptabhead>
   </sp.sptab>
}

/*---------------------------------------------------------------------*/
/*    webdav ...                                                       */
/*---------------------------------------------------------------------*/
service webdav() {
   return <div class="webdav-page">
     <script>
       function webdavToggle() {
	  ${webdavToggle}()
	     .post()
	     .then( v => 
		document.getElementById( "webdav-toggle" )
		       .setAttribute( "data-on", v ? "on" : "off" ) );
       }
     </script>
     
     <div class="webdav-config webdav-head">
       <div class="title">
	 <svg:img class="icon" width="16px" height="16px" 
		  src=${require.resolve( "./icons/hdd.svg" )}/>
       </div>
       <div class="text">
	 Toggle WebDav
	 <div class="comment"> 
	   Enabling or disabling WebDav is only effective after server restart
	 </div>
       </div>
       <div class="button" onclick=~{ webdavToggle() }>
	 <svg:img id="webdav-toggle" 
		  data-on=${config.enableWebdav ? "on" : "off" }
		  class="icon-toggle" width="24px" height="24px" 
		  src=${require.resolve( "./icons/toggle2-off.svg" )}/>
       </div>
     </div>
   </div>;
}
     
/*---------------------------------------------------------------------*/
/*    webdavToggle ...                                                 */
/*---------------------------------------------------------------------*/
service webdavToggle() {
   config.enableWebdav = !config.enableWebdav;
   updateConfig();
   return config.enableWebdav;
}

/*---------------------------------------------------------------------*/
/*    services                                                         */
/*---------------------------------------------------------------------*/
webdav.path = "/hop/hopdroid/webdav";
webdavToggle.path = "/hop/hopdroid/webdav/toggle";
