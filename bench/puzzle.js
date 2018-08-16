"use strict";

const size = 1022;
const classmax = 6;
const typemax = 13;

let III = 0;
let KOUNT = 0;
let D = 8;

const PIECECOUNT = new Array( classmax + 1 );
const CLASS = new Array( typemax + 1 );
const PIECEMAX = new Array( typemax + 1 );
const PUZZLE = new Array( size + 1 );
const P = new Array( typemax + 1 );

PIECECOUNT.fill( 0 );
CLASS.fill( 0 );
PIECEMAX.fill( 0 );

function fit( i, j ) {
   let end = PIECEMAX[ i ];
   let k = 0;

   while( true ) {
      if( k > end || P[ i ][ k ] && PUZZLE[ j + k ] ) {
	 return k > end;
      }
      k++;
   }
   return false;
}

function place( i, j ) {
   let end = PIECEMAX[ i ];
   let k = 0;

   for( ; k <= end; k++ ) {
      if( P[ i ][ k ] ) PUZZLE[ j + k ] = true;
   }
   
   PIECECOUNT[ CLASS[ i ] ] = PIECECOUNT[ CLASS[ i ] ] - 1;

   for( k = j; k <= size && PUZZLE[ k ]; k++ );

   return k > size ? 0 : k;
}

function puzzle_remove( i, j ) {
   let end = PIECEMAX[ i ];

   for( let k = 0; k <= end; k++ ) {
      if( P[ i ][ k ] ) PUZZLE[ j + k ] = false;
   }

   PIECECOUNT[ CLASS[ i ] ] = PIECECOUNT[ CLASS[ i ]  ] + 1;
}

function trial( j ) {
   let k = 0;

   for( let i = 0; i <= typemax; i++ ) {
      if( PIECECOUNT[ CLASS[ i ] ] !== 0 ) {
	 if( fit( i, j ) ) {
	    k = place( i, j );

	    if( trial( k ) || k === 0 ) {
	       KOUNT++;
	       return true;
	    } else {
	       puzzle_remove( i, j );
	    }
	 }
      }
   }

   KOUNT++;
   return false;
}

function trial_output( x, y ) {
   console.log( "piece ", x, " at ", y, "." );
}

function definepiece( iclass, ii, jj, kk ) {
   let index = 0;
   for( let i = 0; i <= ii; i++ ) {
      for( let j = 0; j <= jj; j++ ) {
	 for( let k = 0; k <= kk; k++ ) {
	    index = i + (D * (j + (D * k)));
	    P[ III ][ index ] = true;
	 }
      }
   }

   CLASS[ III ] = iclass;
   PIECEMAX[ III ] = index;

   if( III !== typemax ) III++;
}

function start() {
   for( let m = 0; m <= size; m++ ) {
      PUZZLE[ m ] = true;
   }

   for( let i = 1; i <= 5; i++ ) {
      for( let j = 1; j <= 5; j++ ) {
	 for( let k = 1; k <= 5; k++ ) {
	    PUZZLE[ i + (D * (j + (D * k))) ] = false;
	 }
      }
   }

   for( let i = 0; i <= typemax; i++ ) {
      for( let m = 0; m <= size; m++ ) {
	 P[ i ][ m ] = false;
      }
   }

   III = 0;

   definepiece( 0, 3, 1, 0);
   definepiece( 0, 1, 0, 3);
   definepiece( 0, 0, 3, 1);
   definepiece( 0, 1, 3, 0);
   definepiece( 0, 3, 0, 1);
   definepiece( 0, 0, 1, 3);
   
   definepiece( 1, 2, 0, 0);
   definepiece( 1, 0, 2, 0);
   definepiece( 1, 0, 0, 2);
   
   definepiece( 2, 1, 1, 0);
   definepiece( 2, 1, 0, 1);
   definepiece( 2, 0, 1, 1);
   
   definepiece( 3, 1, 1, 1);

   PIECECOUNT[ 0 ] = 13;
   PIECECOUNT[ 1 ] = 3;
   PIECECOUNT[ 2 ] = 1;
   PIECECOUNT[ 3 ] = 1;

   let m = (D * (D + 1)) + 1;
   let n = 0;

   if( fit( 0, m ) ) {
      n = place( 0, m );
   } else {
      console.log( "error" );
   }

   if( trial( n ) ) {
      console.log( "kount=", KOUNT );
   } else {
      console.log( "failure" );
   }

}

function repeat( num ) {
   while( num-- > 0 ) {
      start();
   }
}

function main() {
   for( let i = 0; i < typemax + 1; i++ ) {
      P[ i ] = new Array( size + 1 );
   }

   repeat( 20 );
}
   
main();   
