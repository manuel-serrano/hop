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
		  
		  if( !("icon" in app) ) {
		     const icon = path.join( p, "etc", "logo.png" );
		     
		     if( fs.existsSync( icon ) ) {
			app.icon = icon;
		     }
		  }

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

/*---------------------------------------------------------------------*/
/*    apps ...                                                         */
/*---------------------------------------------------------------------*/
service apps() {
   const wldir = require( hop.config ).autoloadPath;
   const apps = wldir.flatMap( readApps );
   
   return <div class="apps">
     ${apps.map( APP )}
   </div>;
}

/*---------------------------------------------------------------------*/
/*    app ...                                                          */
/*---------------------------------------------------------------------*/
function APP( a ) {
   return <sp.sptab class="tabapp" svc=${app} arg=${JSON.stringify( a )}>
     <sp.sptabhead>
       <div class="apps-app sptabhead unselected">
	 <div class="app-icon">
	   ${ a.icon ? <img src=${a.icon}/> : "" }
	 </div>
	 <div class="app-title">
	   <div>${a.name}</div>
	   <div class="app-description">${a.description}</div>
	 </div>
       </div>
       <navtitle spageid="spage" class="sphead selected" arrow="&#8672;">
       	 ${a.name}
       </navtitle>
     </sp.sptabhead>
   </sp.sptab>; 
}

/*---------------------------------------------------------------------*/
/*    app ...                                                          */
/*---------------------------------------------------------------------*/
service app( app ) {
   const a = JSON.parse( app );

   return <div class="app-page">
     
     <script>
       function restart( el ) {
	  if( confirm( `Remove Application ${${app.name}}?`) ) {
	     el.setAttribute( "data-hss-reboot", "on" );
	     ${appRemove}( ${app} )
		.post()
		.then( msg => {
		   alert( msg );
		   document.location = "/hop/hopdroid";
		} );
	     setTimeout( () => document.location = "/hop/hopdroid", 3000 );
	  }
       }
     </script>
     
     <div class="app-header">
       <div class="app-icon">
       	 ${ a.icon ? <img src=${a.icon}/> : "" }
       </div>
       <div class="app-title">
       	 ${a.name}
       </div>
     </div>
     
     <div class="app-remove-button" onclick=~{restart( this )}>
       <svg:img class="app-remove" width="24px" height="24px" 
		src=${require.resolve( "./icons/trash.svg" )}/>
       <div class="app-button-text"> Remove App </div>
     </div>
     
     ${serviceInfo( a )}
   </div>
}

/*---------------------------------------------------------------------*/
/*    serviceInfo ...                                                  */
/*---------------------------------------------------------------------*/
function serviceInfo( a ) {
   const { files, size } = statDir( a.directory );
   
   return <div class="app-info">
     <table>
       <tr>
	 <th> version </th> <td> ${a.version} </td>
       </tr>
       <tr>
	 <th> package size </th> <td> ${(size/1024/1024).toFixed( 2 )}MB </td>
       </tr>
       <tr>
	 <th> number of files </th> <td> ${files} </td>
       </tr>
     </table>
   </div>
}

/*---------------------------------------------------------------------*/
/*    statDir ...                                                      */
/*    -------------------------------------------------------------    */
/*    Count the number of files and the cumulated file sizes.          */
/*---------------------------------------------------------------------*/
function statDir( dir ) {
   let files = 0, size = 0;
   
   fs.readdirSync( dir ).forEach( f => {
	 const p = path.join( dir, f );
	 const s = fs.lstatSync( p );
	 
   	 if( s.isDirectory() ) {
	    const { files: sfiles, size: ssize } = statDir( p );
	    files += sfiles;
	    size += ssize;
	 } else {
	    files++;
	    size += s.size;
	 }
      } );
   
   return { files: files, size: size };
}

/*---------------------------------------------------------------------*/
/*    appRemove ...                                                    */
/*---------------------------------------------------------------------*/
service appRemove( app ) {
   console.log( "removing...", app );
}

apps.path = "/hop/hopdroid/apps";
apps.path = "/hop/hopdroid/apps/app";
appRemove.path = "/hop/hopdroid/apps/app/remove";
