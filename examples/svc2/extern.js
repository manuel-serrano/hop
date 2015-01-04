/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/examples/svc2/extern.js           */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Wed May 21 07:50:20 2014                          */
/*    Last change :  Sat Dec 20 10:12:23 2014 (serrano)                */
/*    Copyright   :  2014 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    Basic example that illustrates services declarations.            */
/*    -------------------------------------------------------------    */
/*    run: hop -v -g svc2.js                                           */
/*    browser: http://localhost:8080/hop/svc2                          */
/*=====================================================================*/
service implementation( { a: 10, b: 11 } ) {
   return [ { head: a, data: b }, { head: b, data: a } ];
}

implementation.path = "/hop/dummy";
