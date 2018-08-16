"use strict";

function fib(i) {
   if( i < 2 ) {
      return 1;
   } else {
      return fib( i - 1 ) + fib( i - 2 );
   }
}

let r = 0;

for( let i = 0; i < 5; i++ ) {
   r = fib( 42 );
}

console.log( "fib(42)=", r );

