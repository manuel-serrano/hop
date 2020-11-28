/*=====================================================================*/
/*    serrano/prgm/project/hop/work/hopdroid/xml.js                    */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Wed Nov 25 08:28:29 2020                          */
/*    Last change :  Wed Nov 25 15:17:26 2020 (serrano)                */
/*    Copyright   :  2020 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    hopdroid xml tags                                                */
/*=====================================================================*/
import { NAVTITLE } from './xml.js';

/*---------------------------------------------------------------------*/
/*    NAVTITLE ...                                                     */
/*---------------------------------------------------------------------*/
export function NAVTITLE( attrs, ... nodes ) {
   return <nav class=${(attrs.class || "") + " navtitle"}>
     <ul>
       <li class="nav-back">
	 ${attrs.arrow ?
	    <div class="outer-relief">
	      <button class="nav-back inner-relief"
		      onclick=~{document.getElementById( ${attrs.spageid} ).pop()}>${attrs.arrow}</button>
	    </div> 
	    : <div>
		<svg:img id="main-logo" class="hop-logo" 
		       	 width="2ex" height="2ex" 
		       	 src=${require.resolve( "./hop-sans-style.svg" )}/>
	      </div>}
       </li>
       <li class="nav-title">
	 ${nodes}
       </li>
       <li class="nav-logo">
	 <svg:img id="main" class="hop-logo-sans" 
		  width="2ex" height="2ex" 
		  src=${require.resolve( "./hop-sans-style.svg" )}/> 
       </li>
     </ul>
   </nav>
}
