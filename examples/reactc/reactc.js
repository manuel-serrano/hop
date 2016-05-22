/*=====================================================================*/
/*    serrano/prgm/project/hop/3.1.x/examples/reactc/reactc.js         */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Apr 18 09:42:04 2014                          */
/*    Last change :  Wed May 18 06:03:24 2016 (serrano)                */
/*    Copyright   :  2014-16 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Reactive programming with Hop.js                                 */
/*    -------------------------------------------------------------    */
/*    run: hop -v -g reactc.js                                         */
/*    browser: http://localhost:8080/hop/reactc                        */
/*=====================================================================*/
service react() {
   var tbody = <tbody> <tr><td>toto</td></tr></tbody>;
   return <html>
     ~{ var env = hop.reactProxy( { val: 0 } ); }
     <button onclick=~{
	var el = <react> ~{ <tr><td> tata ${env.val} </td></tr> } </react>;
	${tbody}.appendChild( el );
     }>
	  add element
     </button>
     <button onclick=~{ env.val++; }> increment </button>
      <table border=1> ${tbody} </table>
   </html>
}
	    
console.log( "Go to \"http://%s:%d/hop/reactc\"", hop.hostname, hop.port );
