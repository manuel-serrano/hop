/*=====================================================================*/
/*    serrano/prgm/project/hop/hop/test/hopjs/noserv/aux/class1.js     */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sat Sep  2 01:49:55 2017                          */
/*    Last change :  Sun Jan  2 08:39:08 2022 (serrano)                */
/*    Copyright   :  2017-22 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Exported classes                                                 */
/*=====================================================================*/
"use hopscript";

export { AKS1, AKC1, AKSS2, AKSC2 };
       
class AKS1 {
   x = 100;
}

class AKC1 {
   x = 103;
   xx;
   constructor(xx) {
      this.xx = xx;
   }
}

class AKSS2 extends AKS1 {
   y = 200;
}

class AKSC2 extends AKC1 {
   y = 203;
}


