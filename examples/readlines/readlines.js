/*=====================================================================*/
/*    serrano/prgm/project/hop/hop/examples/readlines/readlines.js     */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Thu Apr 17 08:51:31 2014                          */
/*    Last change :  Sat Oct 14 08:08:16 2023 (serrano)                */
/*    Copyright   :  2014-23 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    READLINES API example                                            */
/*    -------------------------------------------------------------    */
/*    run: hop -v -g readlines.js                                      */
/*    browser: http://localhost:8080/hop/readlines                     */
/*=====================================================================*/
import { openSync as open, closeSync as close } from "fs";
import { readLines } from "hop:readlines";

function ex(f) {
   console.log("reading all lines from", f);
   const fd = open(f, "r");

   if (fd) {
      try {
	 console.log(readLines(fd));
      } finally {
	 close(fd);
      }
   }
}

ex(module.filename);
