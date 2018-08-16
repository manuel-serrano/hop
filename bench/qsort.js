/*=====================================================================*/
/*    serrano/prgm/project/hop/jsbench/micro/qsort.js                  */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Thu Feb  9 14:03:22 2017                          */
/*    Last change :  Fri Mar 17 14:45:00 2017 (serrano)                */
/*    Copyright   :  2017 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    Transcription of the Scheme qsort.scm benchmark.                 */
/*=====================================================================*/
"use strict";

let seed = 0;

function rand() {
   seed = ((seed * 25173) + 17431) & 4095;
   return seed;
}

function qsort( lo, hi, a ) {
   if( lo < hi) {
      let i = lo;
      let j = hi;
      let pivot = a[ hi ];

      while( i < j ) {
	 while( i < hi && a[ i ] <= pivot ) i++;
	 while( j > lo && a[ j ] >= pivot ) j--;

	 if( i < j ) {
	    let temp = a[ i ];
	    a[ i ] = a[ j ];
	    a[ j ] = temp;
	 }
      }

      {
	 let temp = a[ i ];
	 a[ i ] = a[ hi ];
	 a[ hi ] = temp;
      }

      qsort( lo, i - 1, a );
      qsort( i + 1, hi, a );
   }
}


function test( size ) {
   let a = new Array( size );
   let check = new Array( 4096 );

   a[ size - 1 ] = 0;
   
   for( let i = 0; i < check.length; i++ ) check[ i ] = 0;

   for( let j = 0; j <= size - 1; j++ ) {
      let n = rand();
      a[ j ] = n;
      check[ n ]++;
   }

   qsort( 0, size - 1, a );

   check[ a[ 0 ] ]--;

   for( let k = 1; k <= size - 1; k++ ) {
      if( a[ k - 1 ] > a[ k ] ) {
	 throw( "Illegal sort " + k );
      } else {
	 check[ a[ k ] ]--;
      }
   }

   for( let m = 0; m <= 4095; m++ ) {
      if( check[ m ] != 0 ) {
	 throw "Illegal sort " + m;
      }
   }
}

function bench( n ) {
   const size = 1 << 16;
   
   let kkk = Math.round( n / 10 );
   let j = 0;

   for( let i = 0 ; i < n; i++ ) {
      if( i % kkk == 0 ) console.log( j++ );
      test( size );
   }
}

bench( 100 );
