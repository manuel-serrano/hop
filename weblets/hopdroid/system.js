/*=====================================================================*/
/*    serrano/prgm/project/hop/hop/weblets/hopdroid/system.js          */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Thu Nov 26 15:20:46 2020                          */
/*    Last change :  Sun Jan 23 07:42:47 2022 (serrano)                */
/*    Copyright   :  2020-22 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Hopdroid system                                                  */
/*=====================================================================*/
"use hopscript"

import fs from 'fs';
import path from 'path';

import { phone } from './phone.js';
import * as sp from hop.spage;
import { NAVTITLE } from './xml.js';

/*---------------------------------------------------------------------*/
/*    SYSTEM ...                                                       */
/*---------------------------------------------------------------------*/
export function SYSTEM(attrs, ...nodes) {
   return <sp.sptab svc=${system}>
     <sp.sptabhead>
       <nav class="sptabhead unselected">
	 <ul>
	   <li>
	     <div class="icon system-icon">
	       <svg:img class="system-icon" width="16px" height="16px" 
			src=${require.resolve( "./icons/gear.svg" )}/>
	     </div>
	   </li>
	   <li>
	     <div class="title">
	       <div>System</div>
	       <div class="subtitle">system configuration and reset</div>
	     </div>
	   </li>
	 </ul>
       </nav>
       <navtitle spageid="spage" class="sphead selected" arrow="&#8592;">System</navtitle>
     </sp.sptabhead>
   </sp.sptab>
}

/*---------------------------------------------------------------------*/
/*    system ...                                                       */
/*---------------------------------------------------------------------*/
service system() {
   return <div class="system">
     <script>
       function restart( el ) {
	  if( confirm( "Restart Hop server?") ) {
	     el.setAttribute( "data-hss-reboot", "on" );
	     ${systemResetServer}()
		.post()
		.then( e => {
		   if( e ) {
		      alert( e );
		      document.location = "/hop/hopdroid";
		   }
		} );
	     setTimeout( () => document.location = "/hop/hopdroid", 5000 );
	  }
       }
     </script>
     
     <div class="system-reset" onclick=~{restart( this )}>
       <svg:img class="system-restart" width="24px" height="24px" 
		src=${require.resolve( "./icons/exclamation-triangle-fill.svg" )}/>
       <div class="system-button-text"> Restart Server </div>
       <div class="system-reset-spinner">
     	 <div class="system-reset-spinner-cursor">&nbsp;</div>
       </div>
     </div>
   </div>;
}


/*---------------------------------------------------------------------*/
/*    systemResetServer ...                                            */
/*---------------------------------------------------------------------*/
service systemResetServer() {
   try {
      phone.reboot();
      return false;
   } catch( e ) {
      console.error( "systemResetServer: cannot reboot", e.toString() );
      return `Cannot reboot phone ${e.toString()}`;
   }
}

system.path = "/hop/hopdroid/system";
systemResetServer.path = "/hop/hopdroid/system/restart";
