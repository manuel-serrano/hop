/*=====================================================================*/
/*    .../prgm/project/hop/hop/test/hopjs/noserv/aux/record1.js        */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sat Sep  2 01:49:55 2017                          */
/*    Last change :  Sun Jan  2 08:33:29 2022 (serrano)                */
/*    Copyright   :  2017-22 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Exported records                                                 */
/*=====================================================================*/
"use hopscript";

export { ARS1, ARC1, ARSS2, ARSC2 };
       
record ARS1 {
   x = 100;
}

record ARC1 {
   x = 103;
   xx;
   constructor(xx) {
      this.xx = xx;
   }
}

record ARSS2 extends ARS1 {
   y = 200;
}

record ARSC2 extends ARC1 {
   y = 203;
}


