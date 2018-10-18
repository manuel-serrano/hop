service stateful() {
   return <html>
     ~{
        var state = hop.reactProxy( { secondsElapsed: 0 } );
        setInterval( function() { state.secondsElapsed++; }, 1000 );
     }

     <div>Seconds Elapsed: <react>~{ state.secondsElapsed }</react></div>
   </html>
}
