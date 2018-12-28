/*=====================================================================*/
/*    serrano/prgm/project/hop/3.2.x/test/hopjs/noserv/ecma6.js        */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sat Sep 27 10:27:29 2014                          */
/*    Last change :  Fri Dec 28 07:11:33 2018 (serrano)                */
/*    Copyright   :  2014-18 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Testing basic ECMA 262, 6 features                               */
/*=====================================================================*/
var assert = require( "assert" );

/*---------------------------------------------------------------------*/
/*    misc                                                             */
/*---------------------------------------------------------------------*/
function misca() {
   return !new.target;
}

function miscb() {
   let res = false;
   
   function ctor() {
      res = new.target;
   }
   
   new ctor();
   
   return res;
}

function miscc() {
   let res = false;
   
   function aux() {
      return !new.target;
   }
   
   function ctor() {
      res = aux();
   }
   
   new ctor();
   
   return res;
}

function miscd() {
   class kla {
      constructor() {
      	 this.newtarget = new.target;
      }
   }
   
   return new kla().newtarget;
}

function misce() {
   class root {
      constructor() {
      	 this.newtarget = new.target;
      }
   }
      
   class kla extends root {
      constructor( a ) {
	 super();
      }
   }
   
   return new kla().newtarget;
}

function miscf() {
   class root {
      constructor() {
      	 this.newtarget = new.target;
      }
   }
      
   class kla extends root {
   }
   
   return new kla().newtarget;
}

function miscg() {
   class root {
      constructor() {
      	 this.newtarget = new.target;
      }
   }
      
   class kla extends ((function() { return root })()) {
   }
   
   return new kla().newtarget;
}

function misch() {
   class root {
      constructor() {
      	 this.newtarget0 = new.target;
      }
   }
      
   class kla extends ((function() { return root })()) {
      constructor() {
	 super();
      	 this.newtarget1 = new.target;
      }
   }
   
   const o = new kla();
   return o.newtarget0 === o.newtarget1;
}

console.log( "misc" );
console.log( "   misca()"); assert.ok( misca(), "misca" );
console.log( "   miscb()"); assert.ok( miscb(), "miscb" );
console.log( "   miscc()"); assert.ok( miscc(), "miscc" );
console.log( "   miscd()"); assert.ok( miscd(), "miscd" );
console.log( "   misce()"); assert.ok( misce(), "misce" );
console.log( "   miscf()"); assert.ok( miscf(), "miscf" );
console.log( "   miscg()"); assert.ok( miscg(), "miscg" );
