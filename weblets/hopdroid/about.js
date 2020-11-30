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
	     <div class="icon">
	       <svg:img class="about-icon" width="16px" height="16px" 
			src=${require.resolve( "./icons/info-circle.svg" )}/>
	     </div>
	   </li>
	   <li>
	     <div class="title">
	       <div>About Hop and device</div>
	       <div class="subtitle">version</div>
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

   return <div class="about">
     <div class="about-logo">
       <svg:img class="logo" width="128px" height="128px" 
		src=${require.resolve( "./hop-sans-style.svg" )}/>
       <div> Hop v${hop.version} </div>
       <div class="about-phone-model"> ${phone.model} </div>
       <div class="about-phone-product"> ${phone.product} </div>
     </div>
     
     <div class="about-info">
     
       <aboutentry title="ssid"
	       	   value=${wifi.ssid}
	       	   icon=${require.resolve( "./icons/wifi.svg" )}/>
     
       <aboutentry title="IP address"
	       	   value=${wifi.ip}
                   icon=${require.resolve( "./icons/diagram-2.svg" )}/>
     </div>
   </div>
}

/*---------------------------------------------------------------------*/
/*    ABOUTENTRY ...                                                   */
/*---------------------------------------------------------------------*/
function ABOUTENTRY( attr, ... nodes ) {
   return <div class="about-info-entry">
     <svg:img class="icon" width="16px" height="16px" src=${attr.icon}/>
     <div>
       <div class="title">${attr.title}</div> 
       <div class=${`value ${attr.valueclass || ""}`}>${attr.value} </div>
     </div>
   </div>
}
   
