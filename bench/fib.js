"use strict";

function fib(i) {
   if( i < 2 ) {
      return 1;
   } else {
      return fib( i - 1 ) + fib( i - 2 );
   }
}

console.log( "fib=" + fib( 40 ) );

