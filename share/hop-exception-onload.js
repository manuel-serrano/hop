var __onload = undefined;

if (window.hop?.debug) {
   var prop = Object.getOwnPropertyDescriptor( window, "onload" );

   if( "get" in prop ) {
      Object.defineProperty(
	 window, "onload", {
	    get: function() { return __onload },
	    set: function( v ) {
	       __onload = v;
	       prop.set.call( this, hop_callback( v, null, "onload" ) );
	    }
	 } );
   } else {  
      Object.defineProperty(
	 window, "onload", {
	    get: function() { return __onload },
	    set: function( v ) { __onload = v }
	 } );
   }
}

