/*=====================================================================*/
/*    serrano/prgm/project/hop/hop/examples/system/system.js           */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sun Aug 23 08:07:57 2015                          */
/*    Last change :  Mon Feb 20 08:43:11 2023 (serrano)                */
/*    Copyright   :  2015-23 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    An example of SYSTEM                                             */
/*    -------------------------------------------------------------    */
/*    run: hop --no-server -v -g system.js                             */
/*=====================================================================*/

import { systemSync as system } from hop.system;

const statusOrData = system("ls -l /tmp");

if (typeof statusOrData === "number") {
   console.log("error", statusOrData);
} else {
   console.log(statusOrData.split());
}
