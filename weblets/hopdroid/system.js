/*=====================================================================*/
/*    serrano/prgm/project/hop/work/hopdroid/system.js                 */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Thu Nov 26 15:20:46 2020                          */
/*    Last change :  Thu Nov 26 16:47:52 2020 (serrano)                */
/*    Copyright   :  2020 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    Hopdroid system                                                  */
/*=====================================================================*/
var fs = require( 'fs' );
var path = require( 'path' );

import { phone } from './phone.js';
import * as sp from hop.spage;
import { NAVTITLE } from './xml.js';

/*---------------------------------------------------------------------*/
/*    SYSTEM ...                                                       */
/*---------------------------------------------------------------------*/
export function SYSTEM() {
   return <sp.sptab svc=${system}>
     <sp.sptabhead>
       <nav class="sptabhead unselected">
	 <ul>
	   <li>
	     <div class="icon">
	       <svg:img class="system-icon" width="16px" height="16px" 
			src=${require.resolve( "./icons/gear.svg" )}/>
	     </div>
	   </li>
	   <li>
	     <div class="title">
	       <div>System</div>
	       <div class="subtitle">Reset</div>
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
		.then( msg => {
		   alert( msg );
		   document.location = "/hop/hopdroid";
		} );
	     setTimeout( () => document.location = "/hop/hopdroid", 3000 );
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
   } catch( e ) {
      return `Cannot reboot phone ${e.toString()}`;
   }
}

system.path = "/hop/hopdroid/system";
systemResetServer.path = "/hop/hopdroid/system/restart";
