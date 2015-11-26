/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/examples/svc2/extern.js           */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Wed May 21 07:50:20 2014                          */
/*    Last change :  Thu Nov 26 17:08:26 2015 (serrano)                */
/*    Copyright   :  2014-15 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Basic example that illustrates services declarations.            */
/*    -------------------------------------------------------------    */
/*    run: hop -v -g svc2.js                                           */
/*    browser: http://localhost:8080/hop/svc2                          */
/*=====================================================================*/
service implementation( o ) {
   var a = o && "a" in o ? o.a : 10;
   var b = o && "a" in o ? o.b : 11;
   return [ { head: a, data: b }, { head: b, data: a } ];
}

implementation.path = "/hop/dummy";
