/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/examples/widget/widget.js         */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Wed Jun  4 07:54:50 2014                          */
/*    Last change :  Wed Jul 29 21:20:55 2015 (serrano)                */
/*    Copyright   :  2014-15 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Foreign widget binding example                                   */
/*    -------------------------------------------------------------    */
/*    run: hop -v -g widget.js                                         */
/*    browser: http://localhost:8080/hop/widget                        */
/*=====================================================================*/
var hop = require( 'hop' );

var flipcounter = require( "flipcounter" );

service widget() {
   var flip = <flipcounter.FLIPCOUNTER id="myflip"/>;
   
   return <HTML>
      <HEAD jscript=${flipcounter.script} css=${flipcounter.css}/>
      <BUTTON onclick=~{ ${flip}.add( 1 ) }>inc</BUTTON>
      <BUTTON onclick=~{ ${flip}.stop() }>stop</BUTTON>
      <DIV>${flip}</DIV>
   </HTML>;
}
      
console.log( "Go to \"http://%s:%d/hop/widget\"", hop.hostname, hop.port );
