/*=====================================================================*/
/*    serrano/prgm/project/hop/hop/weblets/hopdroid/privacy.js         */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Wed Dec  2 16:36:06 2020                          */
/*    Last change :  Sun Dec 13 06:36:27 2020 (serrano)                */
/*    Copyright   :  2020 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    privacy configuration                                             */
/*=====================================================================*/
"use hopscript"

const fs = require( 'fs' );
const path = require( 'path' );

import { hopdroid } from hop.hopdroid;
import * as sp from hop.spage;
import { NAVTITLE } from './xml.js';
import { config, update as updateConfig } from './config.js';
import * as localConfig from './config.js';

/*---------------------------------------------------------------------*/
/*    authorizeRequest ...                                             */
/*---------------------------------------------------------------------*/
function authorizeRequest( req ) {
   return hop.isLocalRequest( req );
}

/*---------------------------------------------------------------------*/
/*    PRIVACY ...                                                      */
/*---------------------------------------------------------------------*/
export function PRIVACY( attrs ) {
   return <sp.sptab svc=${privacy}>
     <sp.sptabhead>
       <nav class="sptabhead unselected">
	 <ul>
	   <li>
	     <div class="icon privacy-icon">
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
/*    privacy ...                                                      */
/*---------------------------------------------------------------------*/
service privacy() {
   return <div class="privacy-page">
     <script>
       function privacyRemoteToggle() {
	  ${privacyRemoteToggle}()
	     .post()
	     .then( v => {
		if( v.remote && ! v.passwd ) {
		   alert( "Enabling remote access without passwd is dangerous, it exposes the device integrity at hight risk. You should consider adding a password." );
		}
		document.getElementById( "privacy-remote-toggle" )
		   .setAttribute( "data-on", v.remote ? "on" : "off" );
	     } );
       }
     </script>
     
     <div class="privacy-config privacy-head config">
       <div class="title">
	 <svg:img class="icon" width="16px" height="16px" 
		  src=${require.resolve( "./icons/lock.svg" )}/>
       </div>
       <div class="text">
	 Remote access  
	 <div class="comment"> 
	   Enable remote access to the configuration panel
	 </div>
       </div>
       <div class="button" onclick=~{ privacyRemoteToggle() }>
	 <svg:img id="privacy-remote-toggle" 
		  data-on=${config.enableRemote ? "on" : "off" }
		  class="icon-toggle" width="24px" height="24px" 
		  src=${require.resolve( "./icons/toggle-on.svg" )}/>
       </div>
     </div>
     
     <sp.sptab class="tabpasswd" svc=${privacyPasswd}>
     <sp.sptabhead class="privacy-config">
       <div class="config privacy-config privacy-head unselected">
       	 <div class="title">
	   <svg:img class="icon" width="16px" height="16px" 
		    src=${require.resolve( "./icons/key.svg" )}/>
       	 </div>
       	 <div class="text">
	   Password
	   <div class="comment"> 
	     Remote access password
	   </div>
       	 </div>
       </div>
        <navtitle spageid="spage" class="sphead selected" arrow="&#8672;">
       	 passwd
       </navtitle>
     </sp.sptabhead>
     </sp.sptab>
     
   </div>;
}

/*---------------------------------------------------------------------*/
/*    privacyRemoteToggle ...                                          */
/*---------------------------------------------------------------------*/
service privacyRemoteToggle() {
   config.enableRemote = !config.enableRemote;
   updateConfig();
   return { remote: config.enableRemote, passwd: config.passed ? true : false } ;
}

/*---------------------------------------------------------------------*/
/*    privacyPasswd ...                                                */
/*---------------------------------------------------------------------*/
service privacyPasswd() {
   if( hop.isLocalRequest( this ) ) {
      return <div>
	<script>
	  function register( pass ) {
	     ${privacyPasswdRegister}( hop.sha1sum( pass ) )
		.post()
		.then( v => {
		   if( v ) {
		      alert( v );
		   } else {
		      document.getElementById( "spage" ).pop();
		   }
		} )
	  }
	</script>
     	<div class="passwd">
       	  <div class="title">
	    <svg:img class="icon" width="32px" height="32px" 
		     src=${require.resolve( "./icons/lock.svg" )}/>
       	  </div>
	  <div>
       	    Remote Password for device "${phone.name}"
	  </div>
       	  <div>
	    <form>
	      <input type="password"
		     autocomplete="new-password"
		     onkeypress=~{
		      	if( event.code === "Enter" ) {
			   register( this.value );
		      	}
		     }/>
	    </form>
       	  </div>
     	</div>
      </div>
   } else {
      return <div>
     	<div class="passwd">
	  Password can only be changed from with the device
	</div>
      </div>
   }
}

/*---------------------------------------------------------------------*/
/*    privacyPasswdRegister ...                                        */
/*---------------------------------------------------------------------*/
service privacyPasswdRegister( pass ) {
   if( pass.length >= 4 ) {
      localConfig.password = pass;
      return false;
   } else {
      return "Password too short (4 characters or more required)";
   }
}

/*---------------------------------------------------------------------*/
/*    services                                                         */
/*---------------------------------------------------------------------*/
privacy.path = "/hop/hopdroid/privacy";
privacyRemoteToggle.path = "/hop/hopdroid/privacy/remoteToggle";
privacyPasswd.path = "/hop/hopdroid/privacy/passwd";
privacyPasswdRegister.path = "/hop/hopdroid/privacy/passwd/register";
