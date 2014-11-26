/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/examples/requirew/foo.js          */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Oct 24 13:18:22 2014                          */
/*    Last change :  Mon Nov 24 17:55:02 2014 (serrano)                */
/*    Copyright   :  2014 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    A module which will be required via a URL                        */
/*=====================================================================*/

console.log( "in foo.js ", module.filename );

function hello() {
   return <HTML> {
      <DIV> {
	 "I'm an imported function from the source file",
	 <DIV> {
	    <TT> { __filename }
	 }
      }
   };
}

exports.hello = hello;
