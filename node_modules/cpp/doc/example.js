"use strict";

#include "./op.js"

function identity( x ) {
#if( defined( __HOP__ ) )    
   return INC( DEC( x ) );
#else
   return x;
#endif
}

exports.identity = identity;
