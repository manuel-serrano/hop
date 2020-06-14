/*=====================================================================*/
/*    serrano/prgm/project/hop/3.3.x/test/hopjs/noserv/es6-cps.js      */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Oct 30 17:54:07 2015                          */
/*    Last change :  Sat Jun 13 11:26:56 2020 (serrano)                */
/*    Copyright   :  2015-20 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    More es6 generators testing (mostly try/catch/finally)           */
/*=====================================================================*/
"use strict";

const assert = require( "assert" );

function equal( v1, v2 ) {
   if( v1.value == v2.value && v1.done == v2.done ) {
      return true;
   } else {
      console.log( "provided=", v1, "expected=", v2 );
      return false;
   }
}

var g;

/*---------------------------------------------------------------------*/
/*    cps1                                                             */
/*---------------------------------------------------------------------*/
function* cps1() {
   yield 1;
   return 2;
}
	     
console.log( "cps1..." );
g = cps1();
assert.ok( equal( g.next(), { value: 1, done: false } ), "next.0" );
assert.ok( equal( g.next( 3 ), { value: 2, done: true } ), "next.1" );
assert.ok( equal( g.next(), { value: undefined, done: true } ), "next.2" );

/*---------------------------------------------------------------------*/
/*    cps2                                                             */
/*---------------------------------------------------------------------*/
function* cps2() {
   let tmp = 1;
   tmp += yield 2;
   tmp++;
   return tmp;
}
	     
console.log( "cps2..." );
g = cps2();
assert.ok( equal( g.next(), { value: 2, done: false } ), "next.0" );
assert.ok( equal( g.next( 3 ), { value: 5, done: true } ), "next.1" );
assert.ok( equal( g.next(), { value: undefined, done: true } ), "next.2" );

/*---------------------------------------------------------------------*/
/*    cps3                                                             */
/*---------------------------------------------------------------------*/
function* cps3() {
   let tmp = 1;
   if( tmp >= 1 ) {
      tmp += yield 2;
   }
   return ++tmp;
}
	     
console.log( "cps3..." );
g = cps3();
assert.ok( equal( g.next(), { value: 2, done: false } ), "next.0" );
assert.ok( equal( g.next( 3 ), { value: 5, done: true } ), "next.1" );
assert.ok( equal( g.next(), { value: undefined, done: true } ), "next.2" );

/*---------------------------------------------------------------------*/
/*    cps4                                                             */
/*---------------------------------------------------------------------*/
function* cps4() {
   let tmp = 1;
   if( (yield 1) > 2 ) {
      tmp += yield 2;
   }
   return ++tmp;
}
	     
console.log( "cps4..." );
g = cps4();
assert.ok( equal( g.next(), { value: 1, done: false } ), "next.0" );
assert.ok( equal( g.next( 3 ), { value: 2, done: false } ), "next.1" );
assert.ok( equal( g.next( 8 ), { value: 10, done: true } ), "next.2" );
assert.ok( equal( g.next(), { value: undefined, done: true } ), "next.3" );

/*---------------------------------------------------------------------*/
/*    cps5                                                             */
/*---------------------------------------------------------------------*/
function* cps5() {
   let tmp = 1;
   if( (yield 1) > 2 ) {
      tmp += yield 2;
   } else {
      tmp += yield 20;
   }
   return ++tmp;
}
	     
console.log( "cps5..." );
g = cps5();
assert.ok( equal( g.next(), { value: 1, done: false } ), "next.0" );
assert.ok( equal( g.next( 1 ), { value: 20, done: false } ), "next.1" );
assert.ok( equal( g.next( 8 ), { value: 10, done: true } ), "next.2" );
assert.ok( equal( g.next(), { value: undefined, done: true } ), "next.3" );

/*---------------------------------------------------------------------*/
/*    cps6                                                             */
/*---------------------------------------------------------------------*/
function* cps6() {
   let tmp = 1;
   if( (yield 1) === 1 ) {
      tmp += yield 2;
      tmp += yield tmp;
   } else {
      tmp += yield 20;
   }
   return ++tmp;
}
	     
console.log( "cps6..." );
g = cps6();
assert.ok( equal( g.next(), { value: 1, done: false } ), "next.0" );
assert.ok( equal( g.next( 1 ), { value: 2, done: false } ), "next.1" );
assert.ok( equal( g.next( 8 ), { value: 9, done: false } ), "next.2" );
assert.ok( equal( g.next( 3 ), { value: 13, done: true } ), "next.3" );
assert.ok( equal( g.next(), { value: undefined, done: true } ), "next.4" );

/*---------------------------------------------------------------------*/
/*    cps7                                                             */
/*---------------------------------------------------------------------*/
function* cps7() {
   let tmp = 0;
   
   try {
      tmp++;
      yield 1;
      tmp++;
   } catch( e ) {
      tmp += e;
   }
   return ++tmp;
}
	     
console.log( "cps7..." );
g = cps7();
assert.ok( equal( g.next(), { value: 1, done: false } ), "next.0" );
assert.ok( equal( g.throw( 10 ), { value: 12, done: true } ), "throw.1" );
assert.ok( equal( g.next(), { value: undefined, done: true } ), "next.2" );

/*---------------------------------------------------------------------*/
/*    cps8                                                             */
/*---------------------------------------------------------------------*/
function* cps8() {
   let tmp = 0;
   
   try {
      tmp++;
      yield 1;
      tmp++;
   } catch( e ) {
      tmp += e;
      tmp += yield tmp;
   }
   return ++tmp;
}
	     
console.log( "cps8..." );
g = cps8();
assert.ok( equal( g.next(), { value: 1, done: false } ), "next.0" );
assert.ok( equal( g.throw( 10 ), { value: 11, done: false } ), "throw.1" );
assert.ok( equal( g.next( 4 ), { value: 16, done: true } ), "next.2" );
assert.ok( equal( g.next(), { value: undefined, done: true } ), "next.3" );

/*---------------------------------------------------------------------*/
/*    cps9                                                             */
/*---------------------------------------------------------------------*/
function* cps9() {
   let tmp = 0;
   
   try {
      tmp++;
      yield 1;
      tmp++;
      throw 10;
   } catch( e ) {
      tmp += e;
   }
   return ++tmp;
}
	     
console.log( "cps9..." );
g = cps9();
assert.ok( equal( g.next(), { value: 1, done: false } ), "next.0" );
assert.ok( equal( g.next(), { value: 13, done: true } ), "next.1" );

/*---------------------------------------------------------------------*/
/*    cps10                                                            */
/*---------------------------------------------------------------------*/
function* cps10() {
   let tmp = 0;
   
   try {
      try {
      	 tmp++;
      	 yield 1;
      	 tmp++;
      	 throw 10;
      } catch( e ) {
      	 tmp += e;
	 throw 20;
      }
   } catch( e ) {
      tmp += e;
   }
   return ++tmp;
}
	     
console.log( "cps10..." );
g = cps10();
assert.ok( equal( g.next(), { value: 1, done: false } ), "next.0" );
assert.ok( equal( g.next(), { value: 33, done: true } ), "next.1" );
assert.ok( equal( g.next(), { value: undefined, done: true } ), "next.2" );

/*---------------------------------------------------------------------*/
/*    cps11                                                            */
/*---------------------------------------------------------------------*/
function* cps11() {
   let tmp;
   try {
      tmp = 222;
   } catch( e ) {
      lerr = true;
   }
}
	     
console.log( "cps11..." );
g = cps11();

/*---------------------------------------------------------------------*/
/*    cps12                                                            */
/*---------------------------------------------------------------------*/
function* cps12() {
   let tmp = 0;
   try {
      tmp = 6666;
   } finally {
      return 33 + tmp;
   }
}
	     
g = cps12();
assert.ok( equal( g.next(), { value: 6699, done: true } ), "next.0" );
assert.ok( equal( g.next(), { value: undefined, done: true } ), "next.1" );

/*---------------------------------------------------------------------*/
/*    cps13                                                            */
/*---------------------------------------------------------------------*/
function* cps13() {
   let tmp = 0;
   try {
      yield 5555;
      tmp = 6666;
      throw 8888;
   } finally {
      return 33 + tmp;
   }
}
	     
console.log( "cps13..." );
g = cps13();
assert.ok( equal( g.next(), { value: 5555, done: false } ), "next.0" );
assert.ok( equal( g.next(), { value: 6699, done: true } ), "next.1" );
assert.ok( equal( g.next(), { value: undefined, done: true } ), "next.2" );

/*---------------------------------------------------------------------*/
/*    cps14                                                            */
/*---------------------------------------------------------------------*/
function* cps14() {
   let tmp = 0;
   try {
      yield 5555;
      tmp = 6666;
   } finally {
      return 33 + tmp;
   }
}
	     
console.log( "cps14..." );
g = cps14();
assert.ok( equal( g.next(), { value: 5555, done: false } ), "next.0" );
assert.ok( equal( g.next(), { value: 6699, done: true } ), "next.1" );
assert.ok( equal( g.next(), { value: undefined, done: true } ), "next.2" );

/*---------------------------------------------------------------------*/
/*    cps15                                                            */
/*---------------------------------------------------------------------*/
function* cps15() {
   let tmp = 0;
   try {
      try {
      	 yield 5555;
      	 tmp = 6666;
      	 throw 7777;
      } finally {
      	 yield 33 + tmp;
      }
   } catch( e ) {
      return 565;
   }
   return 8888;
}
	     
console.log( "cps15..." );
g = cps15();
assert.ok( equal( g.next(), { value: 5555, done: false } ), "next.0" );
assert.ok( equal( g.next(), { value: 6699, done: false } ), "next.1" );
assert.ok( equal( g.next(), { value: 565, done: true } ), "next.2" );

/*---------------------------------------------------------------------*/
/*    cps16                                                            */
/*---------------------------------------------------------------------*/
function* cps16() {
   let tmp = 0;
   try {
      try {
      	 throw 7777;
      } catch( e ) {
      	 tmp++;
      	 return 565;
      }
   } finally {
      return ++tmp;
   }
   return 777;
}
	     
console.log( "cps16..." );
g = cps16();
assert.ok( equal( g.next(), { value: 2, done: true } ), "next.0" );

/*---------------------------------------------------------------------*/
/*    cps17                                                            */
/*---------------------------------------------------------------------*/
function* cps17() {
   let tmp = 0;
   try {
      try {
      	 yield 5555;
      	 tmp = 6666;
      	 throw 7777;
      } finally {
      	 yield 33 + tmp;
      }
   } catch( e ) {
      return 565;
   } finally {
      yield 8888;
      return 9999;
   }
   return 777;
}
	     
console.log( "cps17..." );
g = cps17();
assert.ok( equal( g.next(), { value: 5555, done: false } ), "next.0" );
assert.ok( equal( g.next(), { value: 6699, done: false } ), "next.1" );
assert.ok( equal( g.next(), { value: 8888, done: false } ), "next.2" );
assert.ok( equal( g.next(), { value: 9999, done: true } ), "next.3" );

/*---------------------------------------------------------------------*/
/*    cps18                                                            */
/*---------------------------------------------------------------------*/
function* cps18() {
   let tmp = 0;
   try {
      try {
      	 yield 5555;
      	 tmp = 6666;
      	 throw 7777;
      } finally {
      	 yield 33 + tmp;
      }
   } catch( e ) {
      yield 1111;
      return 565;
   } finally {
      yield 8888;
      return 9999;
   }
   return 777;
}
	     
console.log( "cps18..." );
g = cps18();
assert.ok( equal( g.next(), { value: 5555, done: false } ), "next.0" );
assert.ok( equal( g.next(), { value: 6699, done: false } ), "next.1" );
assert.ok( equal( g.next(), { value: 1111, done: false } ), "next.2" );
assert.ok( equal( g.next(), { value: 8888, done: false } ), "next.2" );

/*---------------------------------------------------------------------*/
/*    cps19                                                            */
/*---------------------------------------------------------------------*/
function* cps19() {
   let tmp = 0;
   try {
      try {
      	 yield 5555;
      	 tmp = 6666;
      	 throw 7777;
      } finally {
      	 yield 33 + tmp;
      }
   } catch( e ) {
      return 565;
   } finally {
      return 8888;
   }
   return 777;
}
	     
console.log( "cps19..." );
g = cps19();
assert.ok( equal( g.next(), { value: 5555, done: false } ), "next.0" );
assert.ok( equal( g.next(), { value: 6699, done: false } ), "next.1" );
assert.ok( equal( g.next(), { value: 8888, done: true } ), "next.2" );

/*---------------------------------------------------------------------*/
/*    cps20                                                            */
/*---------------------------------------------------------------------*/
function* cps20() {
   let tmp = 0
   let f2 = function() { tmp = 666 };
   let f1 = f2;
   try {
      f1();
   } finally {
      tmp += 33;
   }
   return tmp;
}
	     
g = cps20();
assert.ok( equal( g.next(), { value: 699, done: true } ), "next.0" );
assert.ok( equal( g.next(), { value: undefined, done: true } ), "next.1" );


/*---------------------------------------------------------------------*/
/*    cps21                                                            */
/*---------------------------------------------------------------------*/
function* cps21() {
   try {
      return 555;
   } finally {
      return 666;
   }
   return 777;
}
	    
console.log( "cps21..." );
g = cps21();
assert.ok( equal( g.next(), { value: 666, done: true } ), "next.0" );

/*---------------------------------------------------------------------*/
/*    cps22                                                            */
/*---------------------------------------------------------------------*/
function* cps22() {
   try {
      return 555;
   } finally {
      yield 666;
   }
   return 777;
}
	    
console.log( "cps22..." );
g = cps22();
assert.ok( equal( g.next(), { value: 666, done: false } ), "next.0" );
assert.ok( equal( g.next(), { value: 555, done: true } ), "next.1" );

/*---------------------------------------------------------------------*/
/*    cps23                                                            */
/*---------------------------------------------------------------------*/
function* cps23() {
   let tmp = 0;
   try {
      try {
      	 return 555;
      } finally {
      	 return 666;
      }
   } finally {
      return 888;
   }
   return 777;
}
	    
console.log( "cps23..." );
g = cps23();
assert.ok( equal( g.next(), { value: 888, done: true } ), "next.0" );

/*---------------------------------------------------------------------*/
/*    cps24                                                            */
/*---------------------------------------------------------------------*/
function* cps24() {
   let tmp = 0;
   try {
      yield 555;
   } finally {
      yield 666;
   }
   return 777;
}
	    
console.log( "cps24..." );
g = cps24();
assert.ok( equal( g.next(), { value: 555, done: false } ), "next.0" );
assert.ok( equal( g.next(), { value: 666, done: false } ), "next.1" );
assert.ok( equal( g.next(), { value: 777, done: true } ), "next.2" );

/*---------------------------------------------------------------------*/
/*    cps25                                                            */
/*---------------------------------------------------------------------*/
function* cps25() {
   let tmp = 0;
   try {
      yield 555;
   } finally {
      return 666;
   }
   return 777;
}
	    
console.log( "cps25..." );
g = cps25();
assert.ok( equal( g.next(), { value: 555, done: false } ), "next.0" );
assert.ok( equal( g.next(), { value: 666, done: true } ), "next.1" );

/*---------------------------------------------------------------------*/
/*    cps26                                                            */
/*---------------------------------------------------------------------*/
function* cps26() {
   let tmp = 0;
   try {
      tmp = 6666;
   } finally {
      tmp += 33;
   }
   console.log( "glop" );
   return tmp;
}
	     
g = cps26();
assert.ok( equal( g.next(), { value: 6699, done: true } ), "next.0" );
assert.ok( equal( g.next(), { value: undefined, done: true } ), "next.1" );

/*---------------------------------------------------------------------*/
/*    cps27                                                            */
/*---------------------------------------------------------------------*/
function* cps27() {
   let x = 6666;
   if( x ) {
      x = 3333;
   }
   
   return x;
}
	     
g = cps27();
assert.ok( equal( g.next(), { value: 3333, done: true } ), "next.1" );

/*---------------------------------------------------------------------*/
/*    cps28                                                            */
/*---------------------------------------------------------------------*/
function* cps28() {
   let tmp1 = 10;
   let tmp = tmp1;
   try {
      tmp += 6666;
   } finally {
      tmp += 23;
   }
   console.log( "glop" );
   return tmp;
}
	     
g = cps28();
assert.ok( equal( g.next(), { value: 6699, done: true } ), "next.0" );
assert.ok( equal( g.next(), { value: undefined, done: true } ), "next.1" );

/*---------------------------------------------------------------------*/
/*    cps29                                                            */
/*---------------------------------------------------------------------*/

console.log( "ok" );
