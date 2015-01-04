/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/examples/worker3/bar.js           */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Apr 18 10:09:31 2014                          */
/*    Last change :  Sun Dec 21 11:18:41 2014 (serrano)                */
/*    Copyright   :  2014 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    Services in workers example                                      */
/*    -------------------------------------------------------------    */
/*    run: hop -v -g worker3.js                                        */
/*    browser: http://localhost:8080/hop/worker3                       */
/*=====================================================================*/
var counter = 0;

function html( title ) {
   var count = <SPAN> { id: "counter", counter };
   
   return <HTML> {
      <H1> { title },
      <DIV> { "counter=", count },
      <BUTTON> {
	 onclick: ~{
	    ${service () { return ++counter }}()
	       .post( function( v ) { ${count}.innerHTML = v } )
	 },
	 "inc me"
      }
   }
}

exports.html = html;
