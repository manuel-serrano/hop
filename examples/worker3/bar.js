/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/examples/worker3/bar.js           */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Apr 18 10:09:31 2014                          */
/*    Last change :  Tue Nov  3 11:31:53 2015 (serrano)                */
/*    Copyright   :  2014-15 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Services in workers example                                      */
/*    -------------------------------------------------------------    */
/*    run: hop -v -g worker3.js                                        */
/*    browser: http://localhost:8080/hop/worker3                       */
/*=====================================================================*/
var counter = 0;

function html( title ) {
   var count = <span id="counter">${counter}</span>;
   
   return <html>
     <h1>${title}</h1>
     <div>counter:${count}</div>
     <button onclick=~{
	${service () { return ++counter }}()
	   .post( function( v ) { ${count}.innerHTML = v } ) }>
	inc me
     </button>
   </html>
}

exports.html = html;
