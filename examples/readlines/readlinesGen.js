/*=====================================================================*/
/*    .../prgm/project/hop/hop/examples/readlines/readlinesGen.js      */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Thu Apr 17 08:51:31 2014                          */
/*    Last change :  Sat Oct 14 08:09:46 2023 (serrano)                */
/*    Copyright   :  2014-23 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    READLINES API example                                            */
/*    -------------------------------------------------------------    */
/*    run: hop -v -g readlines.js                                      */
/*    browser: http://localhost:8080/hop/readlines                     */
/*=====================================================================*/
import { openSync as open, closeSync as close } from "fs";
import { readLinesGen } from "hop:readlines";

function ex(f) {
   console.log("reading all lines from", f);
   const fd = open(f, "r");

   if (fd) {
      for (let rl = readLinesGen(fd), l = rl.next(); !l.done; l = rl.next()) {
	 console.log(l.value);
      }
      
      close(json);
   }
}

ex(module.filename);
