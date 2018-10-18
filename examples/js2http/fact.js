var info = { default: 1 };

function fact( x ) {
   if( x <= 1 ) {
      return info.default;
   } else {
      return x * fact( x - 1 );
   }
}

service Fact( x ) {
   return fact( x );
}

console.log( fact( 5 ) );
