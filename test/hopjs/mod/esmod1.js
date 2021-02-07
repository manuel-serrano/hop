"use hopscript"

import { bar } from "./esmod2.js";

export function foo( x ) {
   if( x > 10 ) {
      return bar( x );
   } else {
      return "foo";
   }
}
		    
		    
