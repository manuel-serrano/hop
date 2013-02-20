/*=====================================================================*/
/*    .../arch/android/src/fr/inria/hop/HopPluginMusicPlayer.java      */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Thu Oct 14 08:29:16 2010                          */
/*    Last change :  Wed Feb 20 20:20:40 2013 (serrano)                */
/*    Copyright   :  2010-13 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Android Music Player                                             */
/*    -------------------------------------------------------------    */
/*    This class manages the parsing issues by a Hop broker and to     */
/*    mangage consequently a local Android MediaPlayer.                */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    The package                                                      */
/*---------------------------------------------------------------------*/
package fr.inria.hop;

import android.app.*;
import android.content.*;
import android.os.*;
import android.util.Log;
import android.media.*;
import android.net.*;

import java.net.*;
import java.io.*;

/*---------------------------------------------------------------------*/
/*    The class                                                        */
/*---------------------------------------------------------------------*/
public class HopPluginMusicPlayer extends HopPlugin {
   // global constant
   static final int MPLAYER_STATE_UNSPECIFIED = 0;
   static final int MPLAYER_STATE_INIT = 1;
   static final int MPLAYER_STATE_PLAY = 2;
   static final int MPLAYER_STATE_PAUSE = 3;
   static final int MPLAYER_STATE_STOP = 4;
   static final int MPLAYER_STATE_CLOSE = 5;

   // instance variables
   MediaPlayer mplayer = null;
   int mplayerstate = MPLAYER_STATE_UNSPECIFIED;
   boolean ended = false;
   int mplayervol = 100;
   String datasrc;
   
   // constructor
   public HopPluginMusicPlayer( HopDroid h, String n ) {
      super( h, n );
      Log.v( "HopPluginMusicPlayer", "init mplayer=" + mplayer );
   }

   // create a media player
   private MediaPlayer make_mediaplayer() {
      MediaPlayer mplayer = new MediaPlayer();
      mplayer.setWakeMode( hopdroid.service, PowerManager.PARTIAL_WAKE_LOCK );
      Log.v( "HopPluginMusicPlayer", "********** make mediaplayer..." );
      
      mplayer.setOnPreparedListener( new MediaPlayer.OnPreparedListener() {
	    public void onPrepared( MediaPlayer mp ) {
	       mp.start();
	       Log.v( "HopPlugingMusicPlayer", "start" );
	       Log.v( "HopPlugingMusicPlayer", "mp=" + mp );
	       Log.v( "HopPlugingMusicPlayer", "pos=" + mp.getCurrentPosition() );
	       Log.v( "HopPlugingMusicPlayer", "dur=" + mp.getDuration() );
	       hopdroid.pushEvent( "androidmusic-event", 
				   "(position "
				   + Integer.toString( mp.getCurrentPosition() )
				   + " "
				   + Integer.toString( mp.getDuration() )
				   + ")" );
	       hopdroid.pushEvent( "androidmusic-state", "play" );
	       hopdroid.pushEvent( "androidmusic-volume",
				   Integer.toString( mplayervol ) );

	    }
	 } );

      mplayer.setOnErrorListener( new MediaPlayer.OnErrorListener() {
	    public boolean onError( MediaPlayer mp, int what, int extra ) {
	       Log.v( "HopPluginMusicPlayer", "mediaplayer error: " +
		      what + " " + extra
		      + ", error playing \"" + datasrc + "\"" );
	       hopdroid.pushEvent( "androidmusic-error",
				   "\"error playing \\\"" + datasrc + "\\\"\"" );
	       return false;
	    }
	 } );

      mplayer.setOnInfoListener( new MediaPlayer.OnInfoListener() {
	    public boolean onInfo( MediaPlayer mp, int what, int extra ) {
	       Log.v( "HopPluginMusicPlayer", "mediaplayer info: " +
		      what + " " + extra );
	       if( what == MediaPlayer.MEDIA_INFO_METADATA_UPDATE )
		  return true;
	       else
		  return false;
	    }
	 } );

      mplayer.setOnCompletionListener( new MediaPlayer.OnCompletionListener() {
	    public void onCompletion( MediaPlayer mp ) {
	       hopdroid.pushEvent( "androidmusic-state", "ended" );
	       ended = true;
	    }
	 } );

      mplayer.setOnSeekCompleteListener( new MediaPlayer.OnSeekCompleteListener() {
	    public void onSeekComplete( MediaPlayer mp ) {
	       Log.v( "HopPluginMusicPlayer", "mediaplayer seek complete" );
	       ended = true;
	    }
	 } );

      return mplayer;
   }
   
   // music player
   protected void server( InputStream ip, OutputStream op )
      throws IOException {
      switch( HopDroid.read_int( ip ) ) {
	 case (byte)'x':
	    // exit
	    if( mplayer != null ) {
	       mplayer.release();
	       mplayer = null;
	       mplayerstate = MPLAYER_STATE_CLOSE;	       
	    }
	    return;
	    
	 case (byte)'b':
	    // start
	    Log.v( "HopPluginMusicPlayer", "mediaplayer start" );
	    if( mplayer != null ) {
	       mplayerstate = MPLAYER_STATE_PLAY;	       
	       hopdroid.pushEvent( "androidmusic-state", "play" );
	       mplayer.start();
	    }
	    return;

	 case (byte)'k':
	    // seek
	    if( mplayer != null ) {
	       int sec = HopDroid.read_int32( ip );
	       Log.v( "HopPluginMusicPlayer", "mediaplayer seek: " + sec );
	       mplayer.seekTo( sec * 1000 );
	    }
	    return;

	 case (byte)'e':
	    // stop
	    if( mplayer != null ) {
	       Log.v( "HopPluginMusicPlayer", "mediaplayer set stop" );
	       mplayer.stop();
	       mplayerstate = MPLAYER_STATE_STOP;	       
	       hopdroid.pushEvent( "androidmusic-state", "stop" );
	    }
	    return;
	       
	 case (byte)'p':
	    // pause
	    Log.v( "HopPluginMusicPlayer", "mediaplayer set pause" );
	    if( mplayer != null ) {
	       mplayer.pause();
	       mplayerstate = MPLAYER_STATE_PAUSE;
	       hopdroid.pushEvent( "androidmusic-state", "pause" );
	    }
	    return;

	 case (byte)'u':
	    Log.v( "HopPluginMusicPlayer", "url loading... mplayer" + mplayer );
	    // url
	    if( mplayer == null ) {
	       mplayer = make_mediaplayer();
	    } else {
	       mplayer.reset();
	    }

	    datasrc = HopDroid.read_string( ip );
	    Log.v( "HopPluginMusicPlayer", "datasrc=[" + datasrc + "]" );

	    File file = new File( datasrc );

	    if( file.exists() ) {
	       mplayer.setDataSource( hopdroid.service, Uri.fromFile( file ) );
	       mplayer.prepare();
	       Log.v( "HopPluginMusicPlayer", "start..." );
	       mplayer.start();
	    } else {
	       mplayer.setDataSource( datasrc );
	       mplayer.prepareAsync();
	    }
	    mplayerstate = MPLAYER_STATE_PLAY;	       
	    return;

	 case (byte)'v':
	    // set volume
	    if( mplayer == null ) {
	       mplayer = make_mediaplayer();
	    }

	    int voll = HopDroid.read_int32( ip );
	    int volr = HopDroid.read_int32( ip );

	    Log.v( "HopPluginMusicPlayer", "set volume..." + voll + "/" + volr );

	    mplayervol = (voll + volr) / 2;

	    mplayer.setVolume( (float)voll/100, (float)volr/100 );
	    hopdroid.pushEvent( "androidmusic-volume",
				Integer.toString( mplayervol ) );
	    return;
	    
	 case (byte)'S':
	    // status: state, songlength, songpos
	    synchronized( op ) {
	       if( mplayer == null ) {
		  ended = false;
		  op.write( "(unspecified 0 0)".getBytes() );
	       } else if( mplayerstate == MPLAYER_STATE_CLOSE ) {
		  Log.v( "HopPluginMusicPlayer", "state close" );
		  op.write( "(close 0 0)".getBytes() );
		  break;
	       } else {
		  if( mplayer.isPlaying() ) {
		     ended = false;
		     switch( mplayerstate ) {
			case MPLAYER_STATE_PLAY:
			   Log.v( "HopPluginMusicPlayer", "state play " +
				  Integer.toString( mplayer.getCurrentPosition() / 1000 ) +
				  "/" + Integer.toString( mplayer.getDuration() / 1000 ) );
			   op.write( "(play ".getBytes() );
			   break;
			case MPLAYER_STATE_PAUSE:
			   Log.v( "HopPluginMusicPlayer", "state pause" );
			   op.write( "(pause ".getBytes() );
			   break;
			default:
			   Log.v( "HopPluginMusicPlayer", "state unspecified" );
			   op.write( "(unspecified ".getBytes() );
			   break;
		     }
		     op.write( Integer.toString( mplayer.getDuration() / 1000 ).getBytes() );
		     op.write( " ".getBytes() );
		     op.write( Integer.toString( mplayer.getCurrentPosition() / 1000 ).getBytes() );
		     op.write( ")".getBytes() );
		  } else if( ended ) {
		     Log.v( "HopPluginMusicPlayer", "state ended" );
		     op.write( "(ended 0 0)".getBytes() );
		  } else {
		     Log.v( "HopPluginMusicPlayer", "state stop" );
		     op.write( "(stop 0 0)".getBytes() );
		  }
	       }
	       op.flush();
	    }
	    
	    return;
      }
   }

   // cleanup
   public void kill() {
      super.kill();
      if( mplayer != null && mplayer.isPlaying() ) {
	 mplayer.stop();
	 mplayer.release();
      }
   }
}
