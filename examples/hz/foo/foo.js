/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/examples/hz/foo/foo.js            */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Thu Jul 17 08:01:22 2014                          */
/*    Last change :  Thu Jul 17 08:06:59 2014 (serrano)                */
/*    Copyright   :  2014 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    A minimalist Hop weblet                                          */
/*=====================================================================*/

var hop = require( 'hop' );

service foo() {
   return <HTML> {
      <HEAD> {
	 title: "A minimalist Hop Weblet",
	 favicon: foo.resource( "etc/favicon.png" ),
	 css: foo.resource( "foo.hss" )
      },
      <BODY> {
	 <DIV> {
	    id: "main",
	    "A weblet"
	 }
      }
   }
}
	 
   
