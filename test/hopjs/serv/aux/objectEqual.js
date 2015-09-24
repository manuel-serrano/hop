function objectEqual( a, b ) {
//   console.log( JSON.stringify( a ), JSON.stringify( b ) );
   if ( a === b ) {
      return true;
   };
   if ( typeof( a ) !=  typeof( b ) )
      return false;
   if (Array.isArray( a )) {
      return Array.isArray( b ) && ( a.length == b.length ) &&
	 a.every( function( e, i ) {
	    return objectEqual( e, b[ i ] );
	 } ) &&
	 b.every( function( e, i ) {
	    return objectEqual( e, a[ i ] );
	 } );
   };
   switch (typeof( a )) {
   case 'number':
   case 'string': return false;
   case 'object' :
      for (var key in a) {
	 if (! objectEqual (a[key], b[key]) )
	    return false;
      };
      for (var key in b ) {
	 if( b[key] && (! (a[key]))) 
	    return false;
      };
      return true;
   }
}

/*
console.log(
   objectEqual( 32, 32 ),
   objectEqual( "str", "str" ),
   objectEqual ({foo: {bar: 1, foo: 'str'}, gee: [1, 2, 3]} ,{foo: {bar: 1, foo: 'str'}, gee: [1, 2, 3]} )
);
*/

module.exports = objectEqual;
