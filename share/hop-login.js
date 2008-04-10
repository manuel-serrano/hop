/*=====================================================================*/
/*    serrano/prgm/project/hop/1.9.x/share/hop-login.js                */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sun Mar  9 16:20:17 2008                          */
/*    Last change :  Thu Apr 10 09:52:47 2008 (serrano)                */
/*    Copyright   :  2008 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    Hop login panel                                                  */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    hop_login_prompt ...                                             */
/*---------------------------------------------------------------------*/
function  hop_login_prompt( id, user, pass ) {
   return "<table class='hop-login-prompt'>"
      + " <colgroup><col width='0*'></colgroup>"
      + " <tr><th>Login name</th><td><input type='text' id='hop_login_user_" + id + "'>" + (user ? user : "") + "</input><td></tr>"
      + " <tr><th>Password</th><td><input type='password' id='hop_login_password_" + id + "'>" + (pass ? pass : "") + "</input><td></tr>"
      + " <tr>"
      + "  <td colspan=2>"
      + "   <table class='hop_login_button'>"
      + "    <tr>"
      + "     <td><button class='hop-login-login' id='hop_login_login_" + id + "'>Login</td>"
      + "     <td><button class='hop-login-cancel' id='hop_login_cancel_" + id + "'>Cancel</td>"
      + "    </tr>"
      + "   </table>"
      + "  </td>"
      + " </tr>"
      + "</table";
}

/*---------------------------------------------------------------------*/
/*    hop_login_gensym ...                                             */
/*---------------------------------------------------------------------*/
var hop_login_gensym = 0; 

/*---------------------------------------------------------------------*/
/*    hop_login_class ...                                              */
/*---------------------------------------------------------------------*/
function hop_login_class( base, opt ) {
   if( opt ) {
      return "class='" + base + " " + base + "-" + opt + "'";
   } else {
      return "class='" + base + "'";
   }
}
   
/*---------------------------------------------------------------------*/
/*    hop_make_login_panel ...                                         */
/*---------------------------------------------------------------------*/
function hop_make_login_panel( id, user, pass, klass  ) {
   var login = document.createElement( "div" );
   login.className = "hop-login-panel";

   login.innerHTML =
      "<div " + hop_login_class( 'hop-login-panel-background', klass ) + "></div>"
      + "<div align='center' " + hop_login_class( 'hop-login-main', klass ) + ">"
      + " <table class='hop-login-main-table'>"
      + "  <tr>"
      + "   <td class='hop-login-logo'><div class='hop-login-logo id='hop_login_logo_" + id + "'></div></td>"
      + "   <td>"
      + "    <table class='hop-login-panel'>"
      + "     <tr><td id='hop_login_message_" + id + "'></td></tr>"
      + "     <tr><td>" + hop_login_prompt( id, user, pass ) + "</td></tr>"
      + "    </table>"
      + "   </td>"
      + "  </tr>"
      + " </table>"
      + "</div>";

   return login;
}

/*---------------------------------------------------------------------*/
/*    hop_login_panel ...                                              */
/*---------------------------------------------------------------------*/
/*** META ((export login-panel)) */
function hop_login_panel( args ) {
   var id = hop_login_gensym++;
   var login;
   var klass = false;
   var onlogin, oncancel;
   var logo = false, message = false;
   var user = false, pass = false;
   var i = 0;
   var l = arguments.length;

   while( i < l ) {
      var k = arguments[ i ];

      if( sc_isKeyword( k ) ) {
	 if( i < (l - 1) ) {
	    var at = arguments[ i + 1 ];
	 
	    var s = sc_keyword2jsstring( k );

	    if( s == "logo" ) logo = at;
	    if( s == "message" ) message = at;
	    if( s == "onlogin" ) onlogin = at;
	    if( s == "oncancel" ) oncancel = at;
	    if( s == "user" ) user = at;
	    if( s == "password" ) pass = at;
	    if( s == "class" ) klass = at;

	    i += 2;
	 } else {
	    break;
	 }
      } else {
	 i++;
      }
   }

   if( !klass )
      klass = (hop_config.cpu_speed == 100) ? "fast-gpu" : "slow-gpu";
   
   login = hop_make_login_panel( id, user, pass, klass );
   document.body.appendChild( login );

   ok = document.getElementById( 'hop_login_login_' + id );
   cancel = document.getElementById( 'hop_login_cancel_' + id );

   if( message ) {
      var msg = document.getElementById( 'hop_login_message_' + id );
      if( hop_is_html_element( message ) )
	 msg.appendChild( message );
      else
	 msg.innerHTML = message;
   }

   if( logo ) {
      var l = document.getElementById( 'hop_login_logo_' + id );
      if( hop_is_html_element( logo ) )
	 l.appendChild( logo );
      else
	 l.innerHTML = logo;
   }
      
   hop_add_event_listener( ok, "click", function( e ) {
	 e.user = document.getElementById( "hop_login_user_" + id ).value;
	 e.password = document.getElementById( "hop_login_password_" + id ).value;
	 node_style_set( login, "display", "none" );
	 document.body.removeChild( login );
	 
	 return onlogin( e );
      } );
   
   hop_add_event_listener( cancel, "click", function( e ) {
	 e.user = document.getElementById( "hop_login_user_" + id ).value;
	 e.password = document.getElementById( "hop_login_password_" + id ).value;
	 if( oncancel ) oncancel( e );
	 node_style_set( login, "display", "none" );
	 document.body.removeChild( login );
      } );

   return login;
}
   
