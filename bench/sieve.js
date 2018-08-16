"use strict";

function Cons( a, d ) {
   this.car = a;
   this.cdr = d;
}

function interval( min, max ) {
   if( min > max ) {
      return null;
   } else {
      return new Cons( min, interval( min + 1, max ) );
   }
}

function sfilter( p, l ) {
   if( l === null ) {
      return l;
   } else {
      let a = l.car;
      let r = l.cdr;

      if( p( a ) ) {
	 return new Cons( a, sfilter( p, r ) );
      } else {
	 return sfilter( p, r );
      }
   }
}

function remove_multiples_of( n, l ) {
   return sfilter( m => (m % n) != 0, l );
}

function sieve( max ) {
   function filter_again( l ) {
      if( l === null ) {
	 return l;
      } else {
	 let n = l.car;
	 let r = l.cdr;

	 if( n * n > max ) {
	    return l;
	 } else {
	    return new Cons( n, filter_again( remove_multiples_of( n, r ) ) );
	 }
      }
   }
   return filter_again( interval( 2, max ) );
}

function do_list( f, lst ) {
   while( lst !== null ) {
      f( lst.car );
      lst = lst.cdr;
   }
}

function length( lst ) {
   let res = 0;

   while( lst != null ) {
      res++;
      lst = lst.cdr;
   }

   return res;
}

function doit( num ) {
   let res = 0;
   
   while( num-- > 0 ) {
      res += length( sieve( 3000 ) );
   }

   return res;
}

function list2array( lst ) {
   let len = length( lst );
   var res = new Array( len );

   for( let i = 0; i < len; lst = lst.cdr, i++ ) {
      res[ i ] = lst.car;
   }

   return res;
}
      
const expected_result = [
   2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31,
   37, 41, 43, 47, 53, 59, 61, 67, 71, 73,
   79, 83, 89, 97 ];

function main() {
   let s100 = sieve( 100 );
   let n = 2000;

   doit( n );
   console.log( list2array( s100 ) );
}

main();


