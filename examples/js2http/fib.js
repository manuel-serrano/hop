function fib( x ) {
   if( x < 2 ) {
      return 1;
   } else {
      return fib( x - 1 ) + fib( x - 2 );
   }
}

service Fib( x ) {
   return fib( x );
}

console.log( fib( 20 ) );
