/*=====================================================================*/
/*    .../hop/3.7.x/examples/filebrowser/filebrowser.hop.mjs           */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sat Feb  3 11:25:48 2024                          */
/*    Last change :  Mon Feb  5 10:14:51 2024 (serrano)                */
/*    Copyright   :  2024 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    Basic multi-tier file browser using Hop.js.                      */
/*=====================================================================*/
import * as hop from "@hop/hop";
import { readdir } from "node:fs/promises";
import { statSync as stat } from "node:fs";
import { join, dirname } from "node:path";

const ignoreRx = /^\.|^#.*#$/;

/*---------------------------------------------------------------------*/
/*    R ...                                                            */
/*---------------------------------------------------------------------*/
const R = new hop.Resolver(import.meta.url);

/*---------------------------------------------------------------------*/
/*    dir ...                                                          */
/*---------------------------------------------------------------------*/
async function dirPlain(o) {
   console.log("dir o=", o);
   const dir = o?.path || "/tmp";
   const files = await readdir(dir);
   return <html>
         <head>
            <script type="importmap">
            {
               "imports": {
                  "@hop/hop": "${await R.resolve('@hop/hop/hop-client.mjs')}"
               }
            }
            </script>
            <style>
               .dir {
	          text-decoration: underline;
	          color: blue;
	          cursor: default;
               }
               .file {
	          color: #333;
	          cursor: default;
               }
            </style>
   
         <script type="module">
            import * as hop from "@hop/hop";
            window.filebrowser = ${filebrowser};
            window.filecontent = ${filecontent};
         </script>
      </head>
      <div>
         <span class="dir" onclick=~{location = filebrowser({path: ${dirname(dir)}})}>..</span>      
         <table>
      ${files.filter(p => !p.match(ignoreRx))
	    .sort((x, y) => x >= y)
	    .map(p => {
	       const ap = join(dir, p);
               return <tr><td>
                  ${stat(ap).isDirectory()
                     ? <span class="dir" onclick=~{location = filebrowser({path: ${ap}})}>${p}/</span>
                     : <span class="file" onclick=~{location = filecontent({path: ${ap}})}>${p}</span>}
               </td></tr>
	    })}
         </table>
      </div>
   </html>;
}

/*---------------------------------------------------------------------*/
/*    dir ...                                                          */
/*---------------------------------------------------------------------*/
async function dir(o) {
   console.log("dir o=", o);
   const dir = o?.path || "/tmp";
   const files = await readdir(dir);
   return <html>
         <head>
            <style>
               .dir {
	          text-decoration: underline;
	          color: blue;
	          cursor: default;
               }
               .file {
	          color: #333;
	          cursor: default;
               }
            </style>
   
         <script type="hop">
            window.filebrowser = ${filebrowser};
            window.filecontent = ${filecontent};
         </script>
      </head>
      <div>
         <span class="dir" onclick=~{location = filebrowser({path: ${dirname(dir)}})}>..</span>      
         <table>
      ${files.filter(p => !p.match(ignoreRx))
	    .sort((x, y) => x >= y)
	    .map(p => {
	       const ap = join(dir, p);
               return <tr><td>
                  ${stat(ap).isDirectory()
                     ? <span class="dir" onclick=~{location = filebrowser({path: ${ap}})}>${p}/</span>
                     : <span class="file" onclick=~{location = filecontent({path: ${ap}})}>${p}</span>}
               </td></tr>
	    })}
         </table>
      </div>
   </html>;
}

/*---------------------------------------------------------------------*/
/*    file ...                                                         */
/*---------------------------------------------------------------------*/
async function file(o) {
   const file = o.path;
   return hop.HTTPResponseFile(o);
}
   
const filebrowser = new hop.Service(dir, "/filebrowser");
const filebrowserPlain = new hop.Service(dirPlain, "/filebrowserPlain");
const filecontent = new hop.Service(file, "/filecontent");

console.error("http://localhost:8888/filebrowser");
console.error("config=", hop.config);

// node_modules/@hop/hop/bin/hopc.mjs ./filebrowser.hop.js
// node --no-warnings --enable-source-maps --loader ./node_modules/@hop/hop/lib/hop-loader.mjs ./filebrowser.hop.js
