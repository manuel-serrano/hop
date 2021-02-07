"use hopscript"

import { foo } from "./esmod1.js";

export function bar( x ) { 
   if( x < 10 ) {
      return foo( x );
   } else {
      return "bar";
   }
}
