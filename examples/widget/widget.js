/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/examples/widget/widget.js         */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Wed Jun  4 07:54:50 2014                          */
/*    Last change :  Fri Dec 19 10:30:36 2014 (serrano)                */
/*    Copyright   :  2014 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    Foreign widget binding example                                   */
/*    -------------------------------------------------------------    */
/*    run: hop -v -g widget.js                                         */
/*    browser: http://localhost:8080/hop/widget                        */
/*=====================================================================*/
var hop = require( 'hop' );

var flipcounter = require( "flipcounter" );

service widget() {
   var flip = <flipcounter.FLIPCOUNTER> { id: "myflip" };
   
   return <HTML> {
      <HEAD> {
	 jscript: flipcounter.script,
	 css: flipcounter.css
      },
      <BUTTON> {
	 onclick: ~{ ${flip}.add( 1 ); },
	 "inc"
      },
      <BUTTON> {
	 onclick: ~{ ${flip}.stop(); },
	 "stop"
      },
      <DIV> {
	 flip
      }
   }
}
      
console.log( "Go to \"http://%s:%d/hop/widget\"", hop.hostname, hop.port );
