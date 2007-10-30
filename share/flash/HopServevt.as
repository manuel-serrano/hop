/*=====================================================================*/
/*    serrano/prgm/project/hop/share/flash/HopServevt.as               */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Sep  7 15:31:58 2007                          */
/*    Last change :  Tue Oct 30 06:29:34 2007 (serrano)                */
/*    Copyright   :  2007 Manuel Serrano                               */
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

/*---------------------------------------------------------------------*/
/*    HopServevt ...                                                   */
/*---------------------------------------------------------------------*/
class HopServevt {
   static var app : HopServevt;

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
	    socket.send( "GET /hop/server-event-init?key=" + key
			 + " HTTP/1.1\n\n" );
	 } else {
	    err( "Flash proxy: Cannot establish connection to server" );
	 }
      }
      
      socket.onClose = function() {
	 ExternalInterface.call( onclose );
      }

      socket.onData = function( evt ) {
	 var e = (new XML( evt )).firstChild;

	 if( e.nodeName == "event" ) {
	    var c = e.firstChild;
	    var n = e.attributes.name;

	    if( c.nodeName == "json" ) {
	       ExternalInterface.call( onevent, n, evt, c.firstChild.nodeValue, true );
	    } else {
	       ExternalInterface.call( onevent, n, evt, c.nodeValue, false );
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
      app = new HopServevt( _root.init, _root.port, _root.key,
			    _root.onevent, _root.onclose, _root.onerror );
   }
}
