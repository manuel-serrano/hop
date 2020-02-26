/*=====================================================================*/
/*    serrano/prgm/project/hop/hop/test/hopjs/noserv/ecma6.js          */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sat Sep 27 10:27:29 2014                          */
/*    Last change :  Tue Feb 25 16:14:40 2020 (serrano)                */
/*    Copyright   :  2014-20 Manuel Serrano                            */
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

function misci() {
   var f;

   function F() {
      f =  new.target;
   }

   new F();

   return f === F;
}

function miscj() {
   var f;

   function F() {
      f = () => new.target;
   }

   new F();

   return f() === F;
}

function misck( a ) {
   // test the compilation of const scoping
   if( a > 10 ) {
      const x = a + 4;
      
      function b( y ) {
	 return x + y;
      }
      
      return b( a ) === 44;
   } else {
      return false;
   }
}

console.log( "misc" );
console.log( "   misca()"); assert.ok( misca(), "misca" );
console.log( "   miscb()"); assert.ok( miscb(), "miscb" );
console.log( "   miscc()"); assert.ok( miscc(), "miscc" );
console.log( "   miscd()"); assert.ok( miscd(), "miscd" );
console.log( "   misce()"); assert.ok( misce(), "misce" );
console.log( "   miscf()"); assert.ok( miscf(), "miscf" );
console.log( "   miscg()"); assert.ok( miscg(), "miscg" );
console.log( "   misch()"); assert.ok( misch(), "misch" );
console.log( "   misci()"); assert.ok( misci(), "misci" );
console.log( "   miscj()"); assert.ok( miscj(), "miscj" );
console.log( "   misck()"); assert.ok( misck(), "misck" );

/*---------------------------------------------------------------------*/
/*    mdn                                                              */
/*---------------------------------------------------------------------*/
function mdna() {
   return -(2 ** 2) === -4
      && (2 ** 3) === 8
      && (3 ** 2) === 9
      && (3 ** 2.5) === 15.588457268119896
      && (10 ** -1) === 0.1
      && isNaN( NaN ** 2 )
      && (2 ** 3 ** 2) === 512
      && (2 ** (3 ** 2)) === 512
      && ((2 ** 3) ** 2) === 64;
}

function mdnb() {
   var test = { a: 1 };   
   
   return Object.is('foo', 'foo')
      && !Object.is('foo', 'bar')
      && !Object.is([], [])
      && Object.is(test, test)
      && Object.is(null, null)
      && !Object.is(0, -0)
      && Object.is(-0, -0)
      && Object.is(NaN, 0/0);          
}

console.log( "mdn" );
console.log( "   mdna()"); assert.ok( mdna(), "mdna" );
console.log( "   mdnb()"); assert.ok( mdnb(), "mdnb" );

/*---------------------------------------------------------------------*/
/*    kangax                                                           */
/*---------------------------------------------------------------------*/
function kangaxa() {
   return 2 ** 3 === 8 && -(5 ** 2) === -25 && (-5) ** 2 === 25;
}

function kangaxb() {
   var a = 2; a **= 3; return a === 8;
}

function kangaxc() {
   if (2 ** 3 !== 8) { return false; }
   try {
      Function("-5 ** 2")();
   } catch(e) {
      return true;
   }
}

function kangaxd() {
   return typeof Object.is === 'function' &&
      Object.is(NaN, NaN) &&
      !Object.is(-0, 0);
}
 
console.log( "kangax" );
console.log( "   kangaxa()"); assert.ok( kangaxa(), "kangaxa" );
console.log( "   kangaxb()"); assert.ok( kangaxb(), "kangaxb" );
console.log( "   kangaxc()"); assert.ok( kangaxc(), "kangaxc" );
console.log( "   kangaxd()"); assert.ok( kangaxd(), "kangaxd" );

