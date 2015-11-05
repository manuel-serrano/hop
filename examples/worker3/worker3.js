/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/examples/worker3/worker3.js       */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Apr 18 10:09:31 2014                          */
/*    Last change :  Tue Nov  3 11:33:15 2015 (serrano)                */
/*    Copyright   :  2014-15 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Services in workers example                                      */
/*    -------------------------------------------------------------    */
/*    run: hop -v -g worker3.js                                        */
/*    browser: http://localhost:8080/hop/worker3                       */
/*=====================================================================*/
var hop = require( "hop" );
var bar = require( "./bar.js" );

var w = new Worker( "./slave.js" );

service worker3() {
   import service worker3slave();
   
   var frames = <div style="display: none">
     <iframe style="border: 1px solid black" src=${worker3master()}/>
     <iframe style="border: 1px solid black" src=${worker3slave()}/>
   </div>
   
   return <html>
      ${frames}
      <button onclick=~{
	 this.style.display = "none";
	 ${frames}.style.display = "block" }>
         start
      </button>
   </html>
}
   
service worker3master() {
   return bar.html( "master" );
}

console.log( "Go to \"http://%s:%d/hop/worker3\"", hop.hostname, hop.port );
