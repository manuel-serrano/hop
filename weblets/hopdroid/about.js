/*=====================================================================*/
/*    serrano/prgm/project/hop/work/hopdroid/about.js                  */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Thu Nov 26 15:22:57 2020                          */
/*    Last change :  Thu Nov 26 16:51:19 2020 (serrano)                */
/*    Copyright   :  2020 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    HopDroid about                                                   */
/*=====================================================================*/
"use hopscript"

var fs = require( 'fs' );
var path = require( 'path' );

import { phone } from './phone.js';
import { hopdroid } from hop.hopdroid;
import * as sp from hop.spage;
import { NAVTITLE } from './xml.js';

/*---------------------------------------------------------------------*/
/*    ABOUT ...                                                        */
/*---------------------------------------------------------------------*/
export function ABOUT() {
   return <sp.sptab svc=${about}>
     <sp.sptabhead>
       <nav class="sptabhead unselected">
	 <ul>
	   <li>
	     <div class="icon about-icon">
	       <svg:img class="about-icon" width="16px" height="16px" 
			src=${require.resolve( "./icons/info-circle.svg" )}/>
	     </div>
	   </li>
	   <li>
	     <div class="title">
	       <div>About</div>
	       <div class="subtitle">hop and Device</div>
	     </div>
	   </li>
	 </ul>
       </nav>
       <navtitle spageid="spage" class="selected" arrow="&#8592;">About</navtitle>
     </sp.sptabhead>
   </sp.sptab>
}

/*---------------------------------------------------------------------*/
/*    about ...                                                        */
/*---------------------------------------------------------------------*/
service about() {
   const wifi = new hopdroid.wifi( phone ).info;
   const info = phone.applicationInfo;
   
   return <div class="about">
     <div class="about-logo">
       <svg:img class="logo" width="128px" height="128px" 
		src=${require.resolve( "./hop-sans-style.svg" )}/>
       <div> Hop v${hop.version} </div>
       <div class="about-phone-model"> ${phone.model} ${phone.product} </div>
       <div class="about-phone-product"> ${phone.name} </div>
     </div>
     
     <div class="about-info">
     
       <aboutentry title="ssid"
	       	   value=${wifi.ssid}
	       	   icon=${require.resolve( "./icons/wifi.svg" )}/>
       <aboutentry title="IP address"
	       	   value=${wifi.ip}
                   icon=${require.resolve( "./icons/diagram-2.svg" )}/>
       <aboutentry title="Wi-Fi MAC address"
	       	   value=${wifi.mac}
                   icon=${require.resolve( "./icons/diagram-2-fill.svg" )}/>
       <aboutentry title="Speed"
		   class="section-end"
	       	   value=${wifi.speed}
                   icon=${require.resolve( "./icons/broadcast-pin.svg" )}/>
       <aboutentry title="home directory"
	       	   value=${phone.home}
	       	   icon=${require.resolve( "./icons/phone.svg" )}/>
       <aboutentry title="data directory"
	       	   value=${info["data-dir"]}
		   class="section-end"
	       	   icon=${require.resolve( "./icons/phone-fill.svg" )}/>
     </div>
   </div>
}

/*---------------------------------------------------------------------*/
/*    ABOUTENTRY ...                                                   */
/*---------------------------------------------------------------------*/
function ABOUTENTRY( attr, ... nodes ) {
   return <div class=${`about-info-entry ${attr.class ||""} config`}>
     <svg:img class="icon" width="16px" height="16px" src=${attr.icon}/>
     <div>
       <div class="title">${attr.title}</div> 
       <div class=${`value ${attr.valueclass || ""}`}> ${hop.encodeHTML( attr.value ) } </div>
     </div>
   </div>
}
   
/*---------------------------------------------------------------------*/
/*    services                                                         */
/*---------------------------------------------------------------------*/
about.path = "/hop/hopdroid/about";
