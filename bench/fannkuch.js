"use strict";

function fannkuch(n) {
   var p = [], q = [], s = [];
   var sign = 1, maxflips = 0, sum = 0, m = n-1;
   var tick = 0;

   for(var a=0; a<n; a++){
      p.push(a); q.push(a); s.push(a);
   }

   do {
      // Copy and flip.
      var q0 = p[0];                                     // Cache 0th element.
      if (q0 != 0){
         for(var I0=1; I0<n; I0++) q[I0] = p[I0];             // Work on a copy.
         var flips = 1;
         do {
            var qq = q[q0];
            if (qq === 0){                               // ... until 0th element is 0.
               sum += sign*flips;
	       if (flips > maxflips) maxflips = flips;   // New maximum?
               break;
            }
 	    q[q0] = q0;
	    if (q0 >= 3){
	       var I1 = 1, uuu = q0 - 1, t;
               do { t = q[I1]; q[I1] = q[uuu]; q[uuu] = t; I1++; uuu--; } while (I1 < uuu);
            }
	    q0 = qq;
	    //	    flips++;
	    flips = flips + 1;
         } while (true);
      }
      // Permute.
      if (sign === 1){
         var t = p[1]; p[1] = p[0]; p[0] = t; sign = -1; // Rotate 0<-1.
      } else {
         var t = p[1]; p[1] = p[2]; p[2] = t; sign = 1;  // Rotate 0<-1 and 0<-1<-2.
         for(var I2=2; I2<n; I2++){
	    var sx = s[I2];
	    if (sx != 0){ s[I2] = sx-1; break; }
	    if (I2 === m) return [sum,maxflips];      // Out of permutations.
	    s[I2] = I2;
	    // Rotate 0<-...<-i+1.
	    t = p[0]; for(var jjj=0; jjj<=I2; jjj++){ p[jjj] = p[jjj+1]; } p[I2+1] = t;
         }
      }
   } while (true);

}

var nn = 10;

console.log( "n=", nn );

var k = 0;

for( k = 0; k < 10; k++ ) {
   console.log( "  k=", k );
   var pf = fannkuch(nn);
}

console.log( "pf=", pf );

//console.log(pf[0] + "\n" + "Pfannkuchen(" + n + ") = " + pf[1]);

