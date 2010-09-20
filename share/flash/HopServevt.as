/*=====================================================================*/
/*    serrano/prgm/project/hop/2.2.x/share/flash/HopServevt.as         */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Sep  7 15:31:58 2007                          */
/*    Last change :  Fri Aug  6 17:49:08 2010 (serrano)                */
/*    Copyright   :  2007-10 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    ActionScript server events runtime system.                       */
/*    To be compiled with:                                             */
/*     mtasc -swf HopServevt.swf -main HopServevt.as                   */
/*        -version 9 -cp std8                                          */
/*    The mtasc compiler can be found at: http://www.mtasc.org/        */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    Imports                                                          */
/*---------------------------------------------------------------------*/
import flash.external.ExternalInterface;
import flash.RegExp;

/*---------------------------------------------------------------------*/
/*    HopServevt ...                                                   */
/*---------------------------------------------------------------------*/
class HopServevt {
   static var app = new Array();

   function HopServevt( init, port, key, onevent, onclose, onerror ) {
      var socket:XMLSocket = new XMLSocket();
      
      // debug
      var alert = function( s ) {
	 return ExternalInterface.call( "alert", s );
      }

      var err = function( msg ) {
	 return ExternalInterface.call( onerror, msg );
      }

      var close = function() {
	 socket.close();
      }

      socket.onConnect = function( success ) {
	 if( success ) {
	    socket.send( "GET /hop/server-event/init?key="
			 + key + " HTTP/1.1\n\n" );
	 } else {
	    err( "Flash proxy: Cannot establish connection to server" );
	 }
      }

      socket.onClose = function() {
	 ExternalInterface.call( onclose );
      }

      var encodeStringChar = function( str, char, repl ) {
	 var s = str.split( char );
	 var r = "";
	 var i;

	 for( i = 0; i < s.length - 1; i++ ) r = r.concat( s[ i ] + repl );
	 r = r.concat( s[ i ] );

	 return r;
      }

      var encodeStringSlash = function( str ) {
	 var s = str.split( "\\" );
	 var r = "";
	 var i;

	 for( i = 0; i < s.length - 1; i++ ) r = r.concat( s[ i ] + "\\\\" );
	 r = r.concat( s[ i ] );

	 return encodeStringChar( encodeStringChar( r, "\n", "\\\n" ), "\r", "\\\r" );
      }

      var encodeString = function( str ) {
	 var s = str.split( "\"" );
	 var r = "";
	 var i;

	 for( i = 0; i < s.length - 1; i++ )
	    r = r.concat( encodeStringSlash( s[ i ] ) + '"' );
	 r = r.concat( encodeStringSlash( s[ i ] ) );

	 return r;
      }

      socket.onData = function( evt ) {
	 var e = (new XML( evt )).firstChild;

	 if( e.nodeName == "event" ) {
	    var c = e.firstChild;
	    var n = e.attributes.name;

	    if( c.nodeName == "json" ) {
	       var s = encodeString( c.firstChild.nodeValue );

	       return ExternalInterface.call( onevent, n, "evt", s, true );
	    } else {
	       return ExternalInterface.call( onevent, n, evt, c.nodeValue, false );
	    }
	 }

	 if( e.nodeName == "acknowledge" ) {
	    return ExternalInterface.call( init );
	 }
      }

      ExternalInterface.addCallback( 'close', this, close );

      if( !socket.connect( null, port ) ) {
	 err( "Flash proxy: Cannot open socket" );
      }
   }

   // entry point
   static function main( mc ) {
      //var url = "http://" + _root.host + ":" + _root.port +
      // "/hop/server-event/policy-file?port=" + _root.port +
      // "&key=" + _root.key;

      // CARE: Since Flash 9.0.115, we have to read the security policy file
      // first. The method loadPolicyFile tells the plugin where to find the
      // actual policy file. However, this seems not to work in this version.
      // Until this bug is fixed, the Hop server will keep receiving
      // <file-policy-request/> requests (see http-request.scm). When this
      // bug is fixed, the loadPolicyFile should prevent the
      // <file-policy-request/>.
      //System.security.loadPolicyFile( url );

      // then we can opent the socket
      app.push( new HopServevt( _root.init, _root.port, _root.key,
				_root.onevent, _root.onclose, _root.onerror ) );
   }
}
