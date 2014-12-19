/*=====================================================================*/
/*    .../hop/3.0.x/examples/authentication/authentication.js          */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Apr 18 09:41:35 2014                          */
/*    Last change :  Wed Dec  3 19:10:15 2014 (serrano)                */
/*    Copyright   :  2014 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    show how to ask user authentication                              */
/*    -------------------------------------------------------------    */
/*    run: hop -v -g authentication.js                                 */
/*    browser: http://localhost:8080/hop/authentication                */
/*=====================================================================*/

var hop = require( "hop" );

var count = 2;

service authenticationAccept() {
   switch( count-- ) {
      case 2:
      return hop.HTTPResponseAuthentication( "I don't know you", this );

      case 1:
      return hop.HTTPResponseAuthentication( "Do you really insist?", this );
      
      case 0:
        count = 2;
        return "Ok for this time";
   }
}

service authentication() {
   var console = <DIV> {};

   return <HTML> {
      <DIV> { "Click 3 times the \"click me\" button. Permission granted on the third request." },
      <BUTTON> {
	 onclick: ~{ ${authenticationAccept}()
		     .post( function( v ) { ${console}.innerHTML = v },
			    { fail: function( v ) { ; } } ) },
	 "click me"
      },
      console
   }
}

console.log( "Go to \"http://%s:%d/hop/authentication\"", hop.hostname, hop.port );
