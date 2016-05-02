/*=====================================================================*/
/*    serrano/prgm/project/hop/3.1.x/examples/react/react.js           */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Apr 18 09:42:04 2014                          */
/*    Last change :  Fri Apr 29 19:33:57 2016 (serrano)                */
/*    Copyright   :  2014-16 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Reactive programming with Hop.js                                 */
/*    -------------------------------------------------------------    */
/*    run: hop -v -g react.js                                          */
/*    browser: http://localhost:8080/hop/react                         */
/*=====================================================================*/
service react() {
   return <html>
     ~{
	var width = hop.reactProxy( { val: 2 } );
	var els = hop.reactProxy( ["foo", "bar", "gee" ] );
     }
     
     <button onclick=~{width.val++ }>
       enlarge
     </button>
     <button onclick=~{els.push( "#" + els.length ) }>
       push
     </button>
     
     <table style=~{`border: ${width.val}px solid red`}>
       <react>
       ~{els.map( function( el ) { return <tr><td>1: ${el}</td></tr> } )}
       </react>
     </table>
     <table style=~{`border: ${width.val * 2}px solid green`}>
       <react>
       ~{els.map( function( el ) { return <tr><td>2: ${el}</td></tr> } )}
       </react>
     </table>
   </html>
}

console.log( "Go to \"http://%s:%d/hop/react\"", hop.hostname, hop.port );
