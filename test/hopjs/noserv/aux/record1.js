/*=====================================================================*/
/*    .../prgm/project/hop/hop/test/hopjs/noserv/aux/record1.js        */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sat Sep  2 01:49:55 2017                          */
/*    Last change :  Fri Feb 11 14:39:41 2022 (serrano)                */
/*    Copyright   :  2017-22 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Exported sealed classes                                          */
/*=====================================================================*/
"use hopscript";

export { ARS1, ARC1, ARSS2, ARSC2 };
       
sealed class ARS1 {
   x = 100;
}

sealed class ARC1 {
   x = 103;
   xx;
   constructor(xx) {
      this.xx = xx;
   }
}

sealed class ARSS2 extends ARS1 {
   y = 200;
}

sealed class ARSC2 extends ARC1 {
   y = 203;
}


