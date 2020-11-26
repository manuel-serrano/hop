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
       <navtitle spageid="spage" class="sphead selected" arrow="&#8672;">About</navtitle>
     </sp.sptabhead>
   </sp.sptab>
}

/*---------------------------------------------------------------------*/
/*    about ...                                                        */
/*---------------------------------------------------------------------*/
service about() {
   return <div class="about">
     <div class="about-logo">
       <svg:img class="logo" width="128px" height="128px" 
		src=${require.resolve( "./hop-sans-style.svg" )}/>
       <div> Hop v${hop.version} </div>
       <div class="about-phone-model"> ${phone.model} </div>
       <div class="about-phone-product"> ${phone.product} </div>
     </div>
   </div>;
}

