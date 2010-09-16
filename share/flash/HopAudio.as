/*=====================================================================*/
/*    serrano/prgm/project/hop/2.2.x/share/flash/HopAudio.as           */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Thu Aug 23 16:16:58 2007                          */
/*    Last change :  Fri Aug  6 12:13:59 2010 (serrano)                */
/*    Copyright   :  2007-10 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    HopAudio flash support.                                          */
/*                                                                     */
/*    To be compiled with:                                             */
/*     mtasc -swf HopAudio.swf -main HopAudio.as -version 9 -cp std8   */
/*    The mtasc compiler can be found at: http://www.mtasc.org/        */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    Imports                                                          */
/*---------------------------------------------------------------------*/
import flash.external.ExternalInterface;

/*---------------------------------------------------------------------*/
/*    HopAudio ...                                                     */
/*---------------------------------------------------------------------*/
class HopAudio {
   static var app = new Array();

   function HopAudio( init ) {
      var snd = new Sound();
      var url = false;
      var paused = false;
      var paused_pos = 0;
      var domid = false;
      var onload = false;
      var onerror = false;
      var seek = false;

      // debug
      var alert = function( s ) {
	 return ExternalInterface.call( "alert", s );
      }

      // playSound
      var playSound = function( offset ) {
	 if( url ) {
	    paused = false;
	    snd.start( offset ? offset : 0 );
	    return true;
	 } else {
	    return false;
	 }
      }

      // stopSound
      var stopSound = function() {
	 snd.stop();
	 paused = false;
      }

      // pauseSound
      var pauseSound = function() {
	 if( paused == true ) {
	    paused = false;
	    snd.start( paused_pos );
	 } else {
	    paused_pos = Math.floor( snd.getPosition() / 1000 );
	    paused = true;
	    snd.stop();
	 }
      }

      // loadSound
      var loadSound = function( u, stream ) {
	 url = u;

	 snd.stream = stream;
	 if( snd.getPosition() > 0 ) stopSound();

	 snd.loadSound( u, stream );
      }

      // setVolume
      var setVolume = function( vol ) {
	 return snd.setVolume( vol );
      }

      // getVolume
      var getVolume = function() {
	 return snd.getVolume();
      }

      // setPan
      var setPan = function( pan ) {
	 return snd.setPan( pan );
      }

      // getPan
      var getPan = function() {
	 return snd.getPan();
      }

      // getDuration
      var getDuration = function() {
	 return Math.floor( snd.getDuration() / 1000 );
      }

      // setPosition
      var setPosition = function( position ) {
	 // this is not implemented because snd.start( offset ) just delay
	 // the starting, it does not seek!
      }

      // getPosition
      var getPosition = function() {
	 var pos = snd.getPosition();
	 
	 if( pos > 0 ) {
	    return Math.floor( pos / 1000 );
	 } else {
	    return 0;
	 }
      }

      // getId3
      var getId3 = function() {
	 if( snd.id3 ) {
	    return  "(function(){ var o = new Object; " +
	    "o.title = \"" + snd.id3.songname + "\";" +
	    "o.album = \"" + snd.id3.album + "\";" +
	    "o.year = " + snd.id3.year + ";" +
	    "o.comment = \"" + snd.id3.comment + "\";" +
	    "o.genre = \"" + snd.id3.genre + "\";" +
	    "o.artist = \"" + snd.id3.artist + "\";" +
	    "o.track = " + snd.id3.track + ";" +
	    "return o;})()"
	 } else {
	    return "false";
	 }
      }

      // setOnLoad
      var setOnLoad = function( proc, id ) {
	 onload = proc;
	 domid = id;
      }

      // setOnError
      var setOnError = function( proc, id ) {
	 onerror = proc;
	 domid = id;
      }

      // setOnEnded
      var setOnEnded = function( proc, id ) {
	 snd.onSoundComplete = function() {
	    if( !seek ) ExternalInterface.call( proc, id );
	 }
      }

      // OnLoad Event handlers
      snd.onLoad = function( status ) {
	 if( status ) {
	    if( onload ) {
	       ExternalInterface.call( onload, domid, snd.stream );
	    }
	 } else {
	    if( onerror ) {
	       ExternalInterface.call( onerror, domid, url, "Cannot load stream" );
	    }
	 }
      }

      // initial configuration
      setVolume( 50 );
      
      // External interface binding
      ExternalInterface.addCallback( 'load', this, loadSound );
      ExternalInterface.addCallback( 'flash_play', this, playSound );
      ExternalInterface.addCallback( 'flash_stop', this, stopSound );
      ExternalInterface.addCallback( 'flash_pause', this, pauseSound );
      ExternalInterface.addCallback( 'volume_set', this, setVolume );
      ExternalInterface.addCallback( 'volume_get', this, getVolume );
      ExternalInterface.addCallback( 'pan_set', this, setPan );
      ExternalInterface.addCallback( 'pan_get', this, getPan );
      ExternalInterface.addCallback( 'duration_get', this, getDuration );
      ExternalInterface.addCallback( 'position_set', this, setPosition );
      ExternalInterface.addCallback( 'position_get', this, getPosition );
      ExternalInterface.addCallback( 'id3_get', this, getId3 );
      ExternalInterface.addCallback( 'onload_set', this, setOnLoad );
      ExternalInterface.addCallback( 'onerror_set', this, setOnError );
      ExternalInterface.addCallback( 'onended_set', this, setOnEnded );

      return ExternalInterface.call( init );
   }

   // entry point
   static function main( mc ) {
      app.push( new HopAudio( _root.arg ) );
   }
}
