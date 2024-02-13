/*=====================================================================*/
/*    .../hop/3.7.x/examples/filebrowser/filebrowser.hop.ts            */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sat Feb  3 11:25:48 2024                          */
/*    Last change :  Tue Feb 13 10:30:54 2024 (serrano)                */
/*    Copyright   :  2024 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    Basic multi-tier file browser using Hop.js.                      */
/*=====================================================================*/
import * as hop from "@hop/hop";
import { readdir } from "node:fs/promises";
import { statSync } from "node:fs";
import { join, dirname } from "node:path";

const ignoreRx = /^\.|^#.*#$/;

/*---------------------------------------------------------------------*/
/*    R ...                                                            */
/*---------------------------------------------------------------------*/
const R = new hop.Resolver(import.meta.url);

/*---------------------------------------------------------------------*/
/*    dir ...                                                          */
/*---------------------------------------------------------------------*/
async function dir(o) {
   console.log("dir o=", o);
   const dir = o?.path || "/tmp";
   const files = await readdir(dir);
   return <html>
       <head>
         <link rel="stylesheet" href=${await R.resolve("./filebrowser.css")}>
         <script type="hop">
      console.log("ici", hop.server);
            window.filebrowser = ${filebrowser};
            window.filecontent = ${filecontent};
            window.filestat = ${filestat};
            window.stat = file => {
	       filestat(file).post().then(v => document.getElementById("stat").innerHTML = v);
	    }
         </script>
      </head>
      <div>
         <span class="dir" onclick=~{location = filebrowser({path: ${dirname(dir)}})}>..</span>
         <div id="stat">&nbsp;</div>
         <table>
      ${files.filter(p => !p.match(ignoreRx))
	    .sort((x, y) => x >= y ? 1 : -1 )
	    .map(p => {
	       const ap = join(dir, p);
               return <tr><td>
                  ${statSync(ap).isDirectory()
                     ? <span class="dir" onclick=~{location = filebrowser({path: ${ap}})}>${p}/</span>
                     : <div><span class="file" onclick=~{location = filecontent({path: ${ap}})}>${p}</span> <button onclick=~{stat(${ap})}>stat</button></div>}
               </td></tr>
	    })}
         </table>
      </div>
   </html>;
}

/*---------------------------------------------------------------------*/
/*    file ...                                                         */
/*---------------------------------------------------------------------*/
function file(o) {
   console.log("file o=", o);
   return hop.HTTPResponseFile(o.path);
}

/*---------------------------------------------------------------------*/
/*    stat ...                                                         */
/*---------------------------------------------------------------------*/
function stat(p) {
   console.log("stat p=", p);
   const { size, ctime } = statSync(p);
   return <table>
      <tr><th>size</th><td>${(size/1024).toFixed(2)}kb</td></tr>
      <tr><th>ctime</th><td>${ctime}</td></tr>
   </table>;
}

/*---------------------------------------------------------------------*/
/*    main                                                             */
/*---------------------------------------------------------------------*/
const filebrowser = new hop.Service(dir, "/filebrowser");
const filecontent = new hop.Service(file, "/filebrowser/file");
const filestat = new hop.Service(stat, "/filebrowser/stat");

console.error("http://localhost:8888/filebrowser");
console.error("config=", hop.config);

// node_modules/@hop/hop/bin/hopc.mjs ./filebrowser.hop.js
// node --no-warnings --enable-source-maps --loader ./node_modules/@hop/hop/lib/hop-loader.mjs ./filebrowser.hop.js
