console.log( "bar.js" );

var count = 0;

function count_inc( v ) {
   count += v;
   return count;
}


exports.count = count_inc;

