/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/examples/event/event.js           */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sat Apr 19 07:09:44 2014                          */
/*    Last change :  Wed Dec 17 16:53:55 2014 (serrano)                */
/*    Copyright   :  2014 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    An example of server events.                                     */
/*    -------------------------------------------------------------    */
/*    run: hop -v -g event.js                                          */
/*    browser: http://localhost:8080/hop/event                         */
/*=====================================================================*/
var canvas = <canvas width=550 height=200 style="border: 1px solid black"/>;

service event() {
   return <html>
      ${canvas}
      <br/>
      <button onclick=~{
	 function rndColor() {
	    return Math.round( Math.random() * 255 );
	 }
	 window.open( ${eventSlave}( { red: rndColor(), green: rndColor(), blue: rndColor() } ),
		      "slave-" + count++,
		      "width=600,height=300" );
      }>
	 new window
      </button>

      ~{
	 var count = 0;
	 var canvas = ${canvas};
	 var ctx = canvas.getContext( "2d" );
	 ctx.lineWidth = 10;

	 canvas.addEventListener(
	    "mousedown",
	    function( evt ) {
	       ctx.lineTo( evt.pageX, evt.pageY );
	       ctx.stroke();
	    } );

	 canvas.addEventListener(
	    "mousedown",
	    function( evt ) {
	       var svc = ${service( evt ) {
		  hop.broadcast( "draw", evt );
	       } };
	       svc( { pageX: evt.pageX, pageY: evt.pageY } ).post();
	    } );
      }
   </html>
}

service eventSlave( o ) {
   var red = o && "red" in o ? o.red : 200;
   var green = o && "green" in o ? o.green : 0;
   var blue = o && "blue" in o ? o.blue : 0;
   
   return <html>
      ${canvas}
      ~{
	 window.onload = function() {
	    var canvas = ${canvas};
	    var ctx = canvas.getContext( "2d" );
	    ctx.lineWidth = 15;
	    ctx.strokeStyle = "rgba(" + ${red} + "," + ${green} + "," + ${blue} + ",1)";

	    server.addEventListener(
	       "draw",
	       function( evt ) {
		  console.log( "draw...",  evt );
		  ctx.lineTo( evt.value.pageX, evt.value.pageY );
		  ctx.stroke();
	       } );
	 }
      }
   </html>
}
	    
console.log( "Go to \"http://%s:%d/hop/event\"", hop.hostname, hop.port );
