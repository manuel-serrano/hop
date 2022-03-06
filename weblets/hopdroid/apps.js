/*=====================================================================*/
/*    serrano/prgm/project/hop/hop/weblets/hopdroid/apps.js            */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Wed Nov 25 08:32:40 2020                          */
/*    Last change :  Sun Mar  6 06:30:40 2022 (serrano)                */
/*    Copyright   :  2020-22 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    hopdroid apps                                                    */
/*=====================================================================*/
"use hopscript"

import fs from 'fs';
import path from 'path';

const weblets = require("./_weblets.hop");

import * as sp from hop.spage;
import { NAVTITLE } from './xml.js';

/*---------------------------------------------------------------------*/
/*    service imports                                                  */
/*---------------------------------------------------------------------*/
service hzRemove();
service hzUninstall();
service hzPurge();

/*---------------------------------------------------------------------*/
/*    APPS ...                                                         */
/*---------------------------------------------------------------------*/
export function APPS(attrs, ...nodes) {
   return <sp.sptab svc=${apps}>
     <sp.sptabhead>
       <nav class="sptabhead unselected">
	 <ul>
	   <li>
	     <div class="icon apps-icon">
	       <svg:img class="apps-icon" width="16px" height="16px" 
			src=${require.resolve("./icons/grid-3x3-gap.svg")}/>
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
/*    readDirApps ...                                                  */
/*---------------------------------------------------------------------*/
function readDirApps(dir) {
   function extra(pkg) {
      const p = pkg.prefix;
      
      pkg.ctime = fs.lstatSync(p).ctime;
      if (!("icon" in pkg)) {
	 const icon = path.join(p, "etc", "logo.png");
	 
	 if (fs.existsSync(icon)) {
	    pkg.icon = icon;
	 }
      } else {
	 pkg.icon = path.join(p, pkg.icon);
      }
      return pkg;
   }
   
   if (fs.existsSync(dir) && fs.lstatSync(dir).isDirectory()) {
      return weblets.findWeblets(dir).map(extra);
   } else {
      return [];
   }
}

/*---------------------------------------------------------------------*/
/*    apps ...                                                         */
/*---------------------------------------------------------------------*/
service apps() {
   const wldir = require(hop.config).autoloadPath;
   const apps = wldir
	    .flatMap(readDirApps)
            .sort((x, y) => x.name >= y.name);
   
   return <div class="apps">
     ${apps.map(APP)}
   </div>;
}

/*---------------------------------------------------------------------*/
/*    ICIMG ...                                                        */
/*---------------------------------------------------------------------*/
function ICIMG(attrs) {
   if (attrs.src.match(/[.]svg$/)) {
      return <svg:img src=${attrs.src} width="32px" height="32px"/>;
   } else {
      return <img src=${attrs.src}/>;
   }
}
   
/*---------------------------------------------------------------------*/
/*    APP ...                                                          */
/*---------------------------------------------------------------------*/
function APP(a) {
   return <sp.sptab class="tabapp" svc=${app} arg=${JSON.stringify(a)}>
     <sp.sptabhead>
       <div class="config apps-app sptabhead unselected">
	 <div class="app-icon">
	   ${ a.icon ? <icimg src=${a.icon}/> : "" }
	 </div>
	 <div class="app-title title">
	   <div>${a.name}</div>
	   <div class="app-description">${a.description || a.title || a.comment || "&nbsp;"}</div>
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
service app(app) {
   const a = JSON.parse(app);

   return <div class="app-page">
     
     <script>
       function remove(el) {
	  if (confirm(`Remove Application ${${app.name}}?`)) {
	     ${hzRemove}(${app.name})
		.post()
		.then(err => {
		   if (err) {
		      alert(err);
		   } else {
		      document.getElementById("spage").pop();
		   }
		});
	  }
       }
       
       function purge(el) {
	  if (confirm(`Purge Application ${${app.name}}?`)) {
	     ${hzPurge}(${app.name})
		.post()
		.then(err => {
		   if (err) {
		      alert(err);
		   } else {
		      document.getElementById("spage").pop();
		   }
		});
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
       <div class="app-action-buttons">
       	 <div class="app-action-button" onclick=~{alert("open not implemented yet")}>
       	   <svg:img class="app-action" width="24px" height="24px" 
		    src=${require.resolve("./icons/box-arrow-up-right.svg")}/>
       	   <div class="app-button-text"> OPEN </div>
       	 </div>
       	 <div class="app-action-button" onclick=~{remove(this)}>
       	   <svg:img class="app-action" width="24px" height="24px" 
		    src=${require.resolve("./icons/trash.svg")}/>
       	   <div class="app-button-text"> REMOVE </div>
       	 </div>
       	 <div class="app-action-button" onclick=~{purge(this)}>
       	   <svg:img class="app-action" width="24px" height="24px" 
		    src=${require.resolve("./icons/x-octagon.svg")}/>
       	   <div class="app-button-text"> UNINSTALL </div>
       	 </div>
       </div>
     </div>
     
     ${serviceInfo(a)}
     
   </div>
}

/*---------------------------------------------------------------------*/
/*    serviceInfo ...                                                  */
/*---------------------------------------------------------------------*/
function serviceInfo(a) {
   const { files, size } = statDir(a.prefix);
   
   return <div class="app-info">
     
     <div class="app-info-description">
       ${a.comment || a.description || a.title }
     </div>

     <appentry title="Version"
	       value=${a.version}
	       icon=${require.resolve("./icons/tag.svg")}/>
     
     <appentry title="Installation date"
	       value=${a.ctime}
	       valueclass="valuedate"
	       icon=${require.resolve("./icons/calendar2-check.svg")}/>

     <appentry title="Install directory"
	       value=${a.directory}
	       icon=${require.resolve("./icons/house-door.svg")}/>

     <appentry title="Install size"
	       value=${`${(size/1024/1024).toFixed(2)}MB`}
	       icon=${require.resolve("./icons/calculator.svg")}/>

     <appentry title="Number of installed files"
	       value=${files}
	       icon=${require.resolve("./icons/box-seam.svg")}/>
   </div>
}

/*---------------------------------------------------------------------*/
/*    APPENTRY ...                                                     */
/*---------------------------------------------------------------------*/
function APPENTRY(attr, ... nodes) {
   return <div class="config app-info-entry">
     <svg:img class="icon" width="16px" height="16px" src=${attr.icon}/>
     <div>
       <div class="title">${attr.title}</div> 
       <div class=${`value ${attr.valueclass || ""}`}>${attr.value} </div>
     </div>
   </div>
}
   
/*---------------------------------------------------------------------*/
/*    statDir ...                                                      */
/*    -------------------------------------------------------------    */
/*    Count the number of files and the cumulated file sizes.          */
/*---------------------------------------------------------------------*/
function statDir(dir) {
   let files = 0, size = 0;
   
   fs.readdirSync(dir).forEach(f => {
	 const p = path.join(dir, f);
	 const s = fs.lstatSync(p);
	 
   	 if (s.isDirectory()) {
	    const { files: sfiles, size: ssize } = statDir(p);
	    files += sfiles;
	    size += ssize;
	 } else {
	    files++;
	    size += s.size;
	 }
      });
   
   return { files: files, size: size };
}

/*---------------------------------------------------------------------*/
/*    services                                                         */
/*---------------------------------------------------------------------*/
apps.path = "/hop/hopdroid/apps";
hzRemove.path = "/hop/hz/remove";
hzUninstall.path = "/hop/hz/uninstall";
hzPurge.path = "/hop/hz/purge";

