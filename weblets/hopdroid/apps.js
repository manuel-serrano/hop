/*=====================================================================*/
/*    serrano/prgm/project/hop/work/hopdroid/apps.js                   */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Wed Nov 25 08:32:40 2020                          */
/*    Last change :  Thu Nov 26 15:25:55 2020 (serrano)                */
/*    Copyright   :  2020 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    hopdroid apps                                                    */
/*=====================================================================*/
var fs = require( 'fs' );
var path = require( 'path' );

import * as sp from hop.spage;
import { NAVTITLE } from './xml.js';

/*---------------------------------------------------------------------*/
/*    APPS ...                                                         */
/*---------------------------------------------------------------------*/
export function APPS() {
   return <sp.sptab svc=${apps}>
     <sp.sptabhead>
       <nav class="sptabhead unselected">
	 <ul>
	   <li>
	     <div class="icon">
	       <svg:img class="apps-icon" width="16px" height="16px" 
			src=${require.resolve( "./icons/grid-3x3-gap.svg" )}/>
	     </div>
	   </li>
	   <li>
	     <div class="title">
	       <div>Applications</div>
	       <div class="subtitle">Manage installed applications</div>
	     </div>
	   </li>
	 </ul>
       </nav>
       <navtitle spageid="spage" class="sphead selected" arrow="&#8592;">Applications</navtitle>
     </sp.sptabhead>
   </sp.sptab>
}

/*---------------------------------------------------------------------*/
/*    apps ...                                                         */
/*---------------------------------------------------------------------*/
service apps() {
   const wldir = require( hop.config ).autoloadPath;
   const apps = wldir.flatMap( readApps );

   return <div class="apps">
     ${apps.map( a => <div>${a.name}</div> )}
   </div>;
}

/*---------------------------------------------------------------------*/
/*    readApps ...                                                     */
/*---------------------------------------------------------------------*/
function readApps( dir ) {
   if( fs.lstatSync( dir ).isDirectory() ) {
      return fs.readdirSync( dir )
	 .flatMap( d => { 
	    const p = path.join( dir, d );
	    if( fs.lstatSync( p ).isDirectory() ) {
	       const pkg = path.join( p, "package.json" );
	       
	       if( fs.existsSync( pkg ) && fs.lstatSync( pkg ).isFile ) {
		  const app = require( pkg );
		  app.directory = dir;

		  return [ app ];
	       } else {
		  return [];
	       }
	    } else {
	       return [];
	    }
	 } )
   } else {
      return [];
   }
}
