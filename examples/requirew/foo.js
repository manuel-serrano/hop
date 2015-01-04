/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/examples/requirew/foo.js          */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Oct 24 13:18:22 2014                          */
/*    Last change :  Sat Dec 20 09:51:44 2014 (serrano)                */
/*    Copyright   :  2014 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    A module which will be required via a URL                        */
/*    -------------------------------------------------------------    */
/*    See requirew.js                                                  */
/*=====================================================================*/

console.log( "in foo.js ", module.filename );

function hello() {
   return <HTML> {
      <DIV> {
	 "I'm an imported function from the source file",
	 <DIV> { <TT> { __filename } }
      }
   };
}

exports.hello = hello;
