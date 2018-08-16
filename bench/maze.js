"use strict";

// Adapted by Manuel Serrano from Olin Shivers's maze.scm program
// Copyright (c) 1993 by Olin Shivers.
function Cons( car, cdr ) {
   this.car = car;
   this.cdr = cdr;
}

Cons.prototype.toString = function() {
   return "(" + this.car + (this.cdr instanceof Cons ? " " + this.cdr.toString() : "") + ")";
}

Cons.prototype.toConsString = function() {
   return "(" + this.car + " . " + this.cdr + ")";
}

function CONS( car, cdr ) {
   return new Cons( car, cdr );
}

function Harr( nrows, ncols, elts ) {
   this.nrows = nrows;
   this.ncols = ncols;
   this.elts = elts;
}

Harr.prototype.hrefRC = function( r, c ) {
   return this.elts[ this.ncols * r + c ];
}

Harr.prototype.href = function( x, y ) {
   let r = y / 2 >> 0;
   let c = x / 3 >> 0;

   return this.hrefRC( r, c );
}

function Wall( owner, neighbor, bit ) {
   this.owner = owner;       // Box that owns this wall.
   this.neighbor = neighbor; // The other box bordering this wall.
   this.bit = bit;           // a bit identifying this wall in OWNER's box.
}

function Box( reachable, id, walls = -1, parent = false, mark = false ) {
   this.reachable = reachable; // Union/find set -- all reachable boxs.
   this.id = id;               // Identifying info (e.g., the coords of the box)
   this.walls = walls       // A bitset telling which walls are still standing. 
   this.parent = parent;    // For DFS spanning tree construction.
   this.mark = mark;        // For marking the solution path.
}
   
// Hex arrays
// Copyright (c) 1995 by Olin Shivers.

// External dependencies:
// - define-record

//        ___       ___       ___
//       /   \     /   \     /   \
//   ___/  A  \___/  A  \___/  A  \___
//  /   \     /   \     /   \     /   \
// /  A  \___/  A  \___/  A  \___/  A  \
// \     /   \     /   \     /   \     /
//  \___/     \___/     \___/     \___/
//  /   \     /   \     /   \     /   \
// /     \___/     \___/     \___/     \
// \     /   \     /   \     /   \     /
//  \___/     \___/     \___/     \___/
//  /   \     /   \     /   \     /   \
// /     \___/     \___/     \___/     \
// \     /   \     /   \     /   \     /
//  \___/     \___/     \___/     \___/

// Hex arrays are indexed by the (x,y) coord of the center of the hexagonal
// element. Hexes are three wide and two high; e.g., to get from the center
// of an elt to its {NW, N, NE} neighbors, add {(-3,1), (0,2), (3,1)}
// respectively.
//
// Hex arrays are represented with a matrix, essentially made by shoving the
// odd columns down a half-box so things line up. The mapping is as follows:
//     Center coord      row/column
//     ------------      ----------
//     (x,  y)        -> (y/2, x/3)
//     (3c, 2r + c&1) <- (r,   c)

function harrTabulate( nrows, ncols, proc ) {
   const v = new Array( nrows * ncols );

   for( let r = nrows - 1; !(r < 0); r-- ) {
      for( let c = 0, i = r * ncols; !(c === ncols); c++, i++ ) {
	 v[ i ] = proc( c * 3, 2 * r + (c & 1) );
      }
   }

   return new Harr( nrows, ncols, v );
}


// Hexagonal hackery for maze generation.
// Copyright (c) 1995 by Olin Shivers.

// Every elt of the hex array manages his SW, S, and SE wall.
// Terminology: - An even column is one whose column index is even. That
//                means the first, third, ... columns (indices 0, 2, ...).
//              - An odd column is one whose column index is odd. That
//                means the second, fourth... columns (indices 1, 3, ...).
//              The even/odd flip-flop is confusing; be careful to keep it
//              straight. The *even* columns are the low ones. The *odd*
//              columns are the high ones.
//    _   _
//  _/ \_/ \
// / \_/ \_/
// \_/ \_/ \
// / \_/ \_/
// \_/ \_/ \
// / \_/ \_/
// \_/ \_/ \
// / \_/ \_/
// \_/ \_/
//  0 1 2 3
const SOUTHWEST = 1;
const SOUTH = 2;
const SOUTHEAST = 4;

function genMazeArray( r, c ) {
   return harrTabulate( r, c, (x, y) => new Box( baseSet( 1 ), CONS( x, y ) ) );
}

// This could be made more efficient.
function makeWallVec( harr ) {
   let nrows = harr.nrows;
   let ncols = harr.ncols;
   let xmax = 3 * (ncols - 1);
   let walls = []; // Accumulate walls.

   function addWall( o, n, b ) { // owner neighbor bit
      walls.push( new Wall( o, n, b ) );
   }

   // Do everything but the bottom row.
   for( let x = 3 * (ncols - 1); !(x < 0); x -= 3 ) {
      for( let y = ((nrows - 1) * 2 ) + (x & 1); !(y <= 1); y -= 2 ) {
	 // don't do bottom row.
	 let hex = harr.href( x, y );

	 if( x !== 0 ) addWall( hex, harr.href( x - 3, y - 1 ), SOUTHWEST );
	 addWall( hex, harr.href( x, y - 2 ), SOUTH );
	 if( x < xmax ) addWall( hex, harr.href( x + 3, y - 1 ), SOUTHEAST );
      }
   }

   // Do the SE and SW walls of the odd columns on the bottom row.
   // If the rightmost bottom hex lies in an odd column, however,
   // don't add it's SE wall -- it's a corner hex, and has no SE neighbor.
   if( ncols > 1 ) {
      let rmocX = ~~(3 + (6 * ((ncols - 2) >> 1)));
      let rmocHex = harr.href( rmocX, 1 );

      if( rmocX < xmax ) { // Not a corner == do E wall.
	 addWall( rmocHex, harr.href( xmax, 0 ), SOUTHEAST );
      }
      addWall( rmocHex, harr.href( rmocX - 3, 0 ), SOUTHWEST );
      
      for( let x = rmocX - 6; !(x < 3); x -= 6 ) {
	 addWall( harr.href( x, 1 ), harr.href( x - 3, 0 ), SOUTHWEST );
	 addWall( harr.href( x, 1 ), harr.href( x + 3, 0 ), SOUTHEAST );
      }
   }

   return walls.reverse();
}

let TPLPres = {entrance: false, exit: false };
let BTLPres = { maxlen: false, trance: false, xit: false };

// Find the box ctop from the top row, and the box cbot from the bottom
// row such that cbot is furthest from ctop. 
// Return [ctop-x, ctop-y, cbot-x, cbot-y].
function pickEntrances( harr ) {
   let nrows = harr.nrows;
   let ncols = harr.ncols;
   
   dfsMaze( harr, harr.hrefRC( 0, 0 ), forEachHexChild );

   function TPLP( maxLen, entrance, exit, tcol ) {
/*       console.log( "TPLP maxlen=", maxLen, " entrance=", entrance, " exit=", exit, " tcol=", tcol ); */
      if( tcol < 0 ) {
	 TPLPres.entrance = entrance;
	 TPLPres.exit = exit;
	 
	 return TPLPres;
      } else {
	 let topBox = harr.hrefRC( nrows - 1, tcol );
	 rerootMaze( topBox );

	 function BTLP( maxLen, entrance, exit, bcol ) {
/* 	    console.log( "  BTLP maxlen=", maxLen, " entrance=", entrance, " exit=", exit, " bcol=", bcol ); */
	    if( bcol < 0 ) {
	       BTLPres.maxlen = maxLen;
	       BTLPres.trance = entrance;
	       BTLPres.xit = exit;
	       return BTLPres;
	    } else {
	       const thisLen = pathLength( harr.hrefRC( 0, bcol ) );

	       if( thisLen > maxLen ) {
		  return BTLP( thisLen, tcol, bcol, bcol - 1);
	       } else {
		  return BTLP( maxLen, entrance, exit, bcol - 1);
	       }
	    }
	 }

	 let { maxlen, trance, xit } =
	     BTLP( maxLen, entrance, exit, ncols - 1);

	 return TPLP( maxlen, trance, xit, tcol - 1);
      }
   }

   return TPLP( -1, false, false, ncols - 1);
}

// Apply PROC to each node reachable from BOX.
function forEachHexChild( proc, harr, box ) {
   const walls = box.walls;
   const id = box.id;
   const x = id.car;
   const y = id.cdr;
   const nr = harr.nrows;
   const nc = harr.ncols;
   const maxy = 2 * (nr - 1);
   const maxx = 3 * (nc - 1);

   if( !bitTest( walls, SOUTHWEST ) ) proc( harr.href( x - 3, y - 1 ) );
   if( !bitTest( walls, SOUTH ) ) proc( harr.href( x, y - 2 ) );
   if( !bitTest( walls, SOUTHEAST ) ) proc( harr.href( x + 3, y - 1 ) );

   // NW neighbor, if there is one (we may be in col 1, or top row/odd col)
   if( x > 0 // Not in first column.
       && (y <= maxy // Not on top row or
	   || (x % 6) === 0) ) { // Not in an odd column.
      let nw = harr.href( x - 3, y + 1 );
      if( !bitTest( nw.walls, SOUTHEAST ) ) proc( nw );
   }

   // N neighbor, if there is one (we may be on top row).
   if( y < maxy ) { // Not on top row
      let n = harr.href( x, y + 2 );
      if( !bitTest( n.walls, SOUTH ) ) proc( n );
   }

   // NE neighbor, if there is one (we may be in last col, or top row/odd col)
   if( x < maxx // ; Not in last column.
       && (y <= maxy // Not on top row or
	   || (x % 6) === 0) ) { // ; not in an odd column.
      let ne = harr.href( x + 3, y + 1 );
      if( !bitTest( ne.walls, SOUTHWEST ) ) proc( ne );
   }
}

// The top-level
function makeMaze( nrows, ncols ) {
   let boxs = genMazeArray( nrows, ncols );
   let walls = permuteVec( makeWallVec( boxs ), randomState( 20 ) );

   digMaze( walls, nrows * ncols );

   let { entrance, exit } = pickEntrances( boxs );

   let exitBox = boxs.hrefRC( 0, exit );
   let nwalls = exitBox.walls;

   rerootMaze( boxs.hrefRC( nrows - 1, entrance ) );
   markPath( exitBox );
   exitBox.walls = nwalls & ~SOUTH;

   return { boxs, entrance, exit };
}

function pmaze( nrows, ncols ) {
   let { boxs, entrance, exit } = makeMaze( nrows, ncols );

   printHexMaze( boxs, entrance );
}

// Print out a hex array with characters.
// Copyright (c) 1995 by Olin Shivers.

// External dependencies:
// - hex array code
// - hex box code

//    _   _
//  _/ \_/ \
// / \_/ \_/
// \_/ \_/ \
// / \_/ \_/
// \_/ \_/ \
// / \_/ \_/
// \_/ \_/ \
// / \_/ \_/
// \_/ \_/ 

// Top part of top row looks like this:
//    _   _  _   _
//  _/ \_/ \/ \_/ \
// /
function printHexMaze( harr, entrance ) {
   let nrows = harr.nrows;
   let ncols = harr.ncols;
   let ncols2 = (ncols >> 1) << 1;

   // Print out the flat tops for the top row's odd cols.
   for( let c = 1; !(c >= ncols); c+= 2 ) {
      display( "   " );
      writeChar( c === entrance ? " " : "_" );
   }
   newline();

   // Print out the slanted tops for the top row's odd cols
   // and the flat tops for the top row's even cols.
   writeChar( " " );
   for( let c = 0; !(c >= ncols2); c += 2 ) {
      display( c === entrance ? " " : "_" );
      display( "/" );
      display( dotSpace( harr, nrows - 1, c + 1 ) );
      display( "\\" );
   }

   if( ncols & 1 ) writeChar( entrance === (ncols - 1) ? " " : "_" );
   newline();

   for( let r = nrows - 1; !(r < 0); r-- ) {
      // Do the bottoms for row r's odd cols.
      writeChar( "/" );
      for( let c = 1; !(c >= ncols2); c += 2 ) {
	 // The dot/space for the even col just behind c.
	 writeChar( dotSpace( harr, r, c - 1 ) );
	 displayHexBottom( harr.hrefRC( r, c ).walls );
      }

      if( ncols & 1 ) {
	 writeChar( dotSpace( harr, r, ncols - 1 ) );
	 writeChar( "\\" );
      }

      newline();

      // Do the bottoms for row r's even cols.
      for( let c = 0; !(c >= ncols2); c += 2 ) {
	 displayHexBottom( harr.hrefRC( r, c ).walls );
	 // The dot/space is for the odd col just after c, on row below.
	 writeChar( dotSpace( harr, r - 1, c + 1 ) );
      }

      if( ncols & 1 ) {
	 displayHexBottom( harr.hrefRC( r, ncols - 1 ).walls );
      } else if( r !== 0 ) {
	 writeChar( "\\" );
      }

      newline();
   }
}

function bitTest( j, bit ) {
   return (j & bit) !== 0;
}

// Return a . if harr[r,c] is marked, otherwise a space.
// We use the dot to mark the solution path.
function dotSpace( harr, r, c ) {
   return ( r >= 0 && harr.hrefRC( r, c ).mark ) ? "." : " ";
}

// Print a \_/ hex bottom.
function displayHexBottom( hexwalls ) {
   writeChar( bitTest( hexwalls, SOUTHWEST ) ? "\\" : " " );
   writeChar( bitTest( hexwalls, SOUTH ) ? "_" : " " );
   writeChar( bitTest( hexwalls, SOUTHEAST ) ? "/" : " " );
}

//    _   _
//  _/ \_/ \
// / \_/ \_/
// \_/ \_/ \_/
// / \_/ \_/
// \_/ \_/ \
// / \_/ \_/
// \_/ \_/ \
// / \_/ \_/
// \_/ \_/ \_/


// Randomly permute a vector.
function permuteVec( v, randomState ) {
   for( let i = v.length - 1; i > 1; i-- ) {
      let eli = v[ i ];
      let j = randomInt( i, randomState );

      v[ i ] = v[ j ];
      v[ j ] = eli;
   }

   return v;
}

Array.prototype.forEachRev = function( proc ) {
   for( let i = this.length - 1; i >= 0; i-- ) {
      proc( this[ i ] );
   }
}
   
// This is the core of the algorithm.
function digMaze( walls, nboxs ) {
   try {
      walls.forEachRev( wall => { // For each wall,
	 let c1 = wall.owner;     // find the boxs on
	 let set1 = c1.reachable;
	 let c2 = wall.neighbor; // each side of the wall
	 let set2 = c2.reachable;

	 // If there is no path from c1 to c2, knock down the
	 // wall and union the two sets of reachable boxs.
	 // If the new set of reachable boxs is the whole set
	 // of boxs, quit.
	 if( !setEqual( set1, set2 ) ) {
	    let nwalls = c1.walls;
	    let wallmask = ~wall.bit;
	    union( set1, set2 );

	    c1.walls = nwalls & wallmask;
	    if( setSize( set1 ) === nboxs ) {
	       throw false;
	    }
	 }
      } );
   } catch( exn ) {
      if( exn ) throw exn;
      ;
   }
}

// Some simple DFS routines useful for determining path length 
// through the maze.

// Build a DFS tree from ROOT. 
// (DO-CHILDREN proc maze node) applies PROC to each of NODE's children.
// We assume there are no loops in the maze; if this is incorrect, the
// algorithm will diverge.
function dfsMaze( maze, root, doChildren ) {
   function search( node, parent ) {
      node.parent = parent;
      return doChildren( child => {
	 if( child !== parent ) search( child, node );
      }, maze, node );
   }

   return search( root, false );
}

// Move the root to NEW-ROOT
function rerootMaze( newRoot ) {
   function lp( node, newParent ) {
      let oldParent = node.parent;
      node.parent = newParent;
      if( oldParent ) lp( oldParent, node );
   }

   return lp( newRoot, false );
}

//  How far from BOX to the root?
function pathLength( box ) {
   let len = 0;
   let node = box.parent;

   for( ; !!node; len++, node = node.parent );

   return len;
}

// Mark the nodes from NODE back to root. Used to mark the winning path.
function markPath( node ) {
   node.mark = true;
   if( node.parent ) markPath( node.parent );
}

// Minimal Standard Random Number Generator
// Park & Miller, CACM 31(10), Oct 1988, 32 bit integer version.
// better constants, as proposed by Park.
// By Ozan Yigit

// Rehacked by Olin 4/1995.

function fx28( a ) {
   return ((1 << 28) - 1) & a;
}

// randomState
function *randomState( n ) {
   const A = 48271;
   const M = 268435455;
   const Q = 44488;
   const R = 3399;
   let seed = n;

   while( true ) {
      let hi = fx28( seed / Q );
      let lo = fx28( seed % Q );
      let test = fx28( fx28( A * lo ) - fx28( R * hi ) );
      let val = test > 0 ? test : fx28( test + M );

      yield val;
      seed = val;
   }
}

function rand( state ) {
   return state.next().value;
}
   
// rand

// randomInt
function randomInt( n, state ) {
   return fx28( rand( state ) % n );
}

// Tarjan's amortised union-find data structure.
// Copyright (c) 1995 by Olin Shivers.

// This data structure implements disjoint sets of elements.
// Four operations are supported. The implementation is extremely
// fast -- any sequence of N operations can be performed in time
// so close to linear it's laughable how close it is. See your
// intro data structures book for more. The operations are:
//
// - (base-set nelts) -> set
//   Returns a new set, of size NELTS.
//
// - (set-size s) -> integer
//   Returns the number of elements in set S.
//
// - (union! set1 set2)
//   Unions the two sets -- SET1 and SET2 are now considered the same set
//   by SET-EQUAL?.
//
// - (set-equal? set1 set2)
//   Returns true <==> the two sets are the same.

// Representation: a set is a cons box. Every set has a "representative"
// cons box, reached by chasing cdr links until we find the cons with
// cdr = (). Set equality is determined by comparing representatives using
// EQ?. A representative's car contains the number of elements in the set.

// The speed of the algorithm comes because when we chase links to find 
// representatives, we collapse links by changing all the boxs in the path
// we followed to point directly to the representative, so that next time
// we walk the cdr-chain, we'll go directly to the representative in one hop.
function baseSet( nelts ) {
   return CONS( nelts, null );
}

// Sets are chained together through cdr links. Last guy in the chain
// is the root of the set.
function getSetRoot( s ) {
   function lp( r ) { // Find the last pair
      let next = r.cdr; // in the list. That's
      if( next ) { // the root r.
	 return lp( next );
      } else {
	 if( r !== s ) { // Now zip down the list again,
	    function lq( x ) {  // changing everyone's cdr to r.
	       let next = x.cdr;
	       if( r !== next ) {
		  x.cdr = r;
		  return lq( next );
	       }
	    }

	    lq( s );
	 }

	 return r;
      }
   }

   return lp( s );
}

function setEqual( s1, s2 ) {
   return getSetRoot( s1 ) === getSetRoot( s2 );
}

function setSize( s ) {
   return getSetRoot( s ).car;
}

function union( s1, s2 ) {
   let r1 = getSetRoot( s1 );
   let r2 = getSetRoot( s2 );
   let n1 = setSize( r1 );
   let n2 = setSize( r2 );
   let n = n1 + n2;

   if( n1 > n2 ) {
      r2.cdr = r1;
      r1.car = n;
   } else {
      r1.cdr = r2;
      r2.car = n;
   }
}

var stdio = true;

function display( str ) {
   if( stdio != null ) {
      process.stdout.write( str );
   }
}

function writeChar( char ) {
   return display( char );
}

function newline() {
   return display( "\n" );
}

function run( num ) {
   let k = num / 10;

   stdio = null;
   for( let i = 0; i < num; i++ ) {
      pmaze( 500, 35 );
      if( i % k == 0 ) console.log( i );
   }

   stdio = true;
      
   pmaze( 10, 35 );

}

run( 20 );
