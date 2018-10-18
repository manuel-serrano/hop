service ui() {
   return <html>
     ~{
	var R = hop.reactProxy( { min: 0, max: 100 } );
	var mid;
     }

     <body>
       <react> ~{
	  if( R.min == R.max ) {
	     return <div>Got it: ${R.min}!
	       <button onclick=~{R.min = 0; R.max = 100}>again</button>
	     </div>
	  } else if( R.min == 0 ) {
	     return <div>Think of an integer between ${R.min + 1} and ${R.max}
	       <button onclick=~{
		  R.min = 1; R.max = 100;
	       }>Ok
	       </button>
	     </div>
	  } else {
	     mid = R.min + Math.round((R.max-R.min)/2);
	     return <div>Is it smaller than ${mid}?
	       <button onclick=~{R.max = mid - 1 }>Yes</button>
	       <button onclick=~{R.min = mid }>No</button>
	     </div>
	  }
       }
       </react>
     </body>
   </html>
}


