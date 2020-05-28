/*=====================================================================*/
/*    serrano/prgm/project/hop/3.2.x/examples/resource/resource.js     */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Apr 18 09:42:04 2014                          */
/*    Last change :  Tue Jan 29 08:42:37 2019 (serrano)                */
/*    Copyright   :  2014-19 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    basic example that shows how to access local resources           */
/*    -------------------------------------------------------------    */
/*    run: hop -v -g resource.js                                       */
/*    browser: http://localhost:8080/hop/resource                      */
/*=====================================================================*/
service resource() {
   return <html>
     <ul>
       <li> An image accessed via an absolute path:
          <img src=${require.resolve( "./hop.png" )}/>
       </li>
       <li> An image accessed via a service:
          <img src=${getResource( "./hop.png" )}/>
       </li>
     </ul>  
   </html>
}

service getResource( path ) {
   return hop.HTTPResponseFile( require.resolve( path ) );
}
   
console.log( "Go to \"http://%s:%d/hop/resource\"", hop.hostname, hop.port );
