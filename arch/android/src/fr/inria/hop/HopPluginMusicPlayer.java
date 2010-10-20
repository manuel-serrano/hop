/*=====================================================================*/
/*    .../arch/android/src/fr/inria/hop/HopPluginMusicPlayer.java      */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Thu Oct 14 08:29:16 2010                          */
/*    Last change :  Tue Oct 19 19:07:55 2010 (serrano)                */
/*    Copyright   :  2010 Manuel Serrano                               */
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

   // constructor
   public HopPluginMusicPlayer( HopAndroid h, Activity a, String n ) {
      super( h, a, n );
   }
   
   // create a media player
   private MediaPlayer make_mediaplayer() {
      MediaPlayer mplayer = new MediaPlayer();
      
      mplayer.setOnPreparedListener( new MediaPlayer.OnPreparedListener() {
	    public void onPrepared( MediaPlayer mp ) {
	       Log.v( "HopAndroidMusicPlayer", "mediaplayer prepared." );
	       mp.start();
	    }
	 } );

      mplayer.setOnErrorListener( new MediaPlayer.OnErrorListener() {
	    public boolean onError( MediaPlayer mp, int what, int extra ) {
	       Log.v( "HopAndroidMusicPlayer", "mediaplayer error: " +
		      what + " " + extra );
	       return false;
	    }
	 } );

      mplayer.setOnInfoListener( new MediaPlayer.OnInfoListener() {
	    public boolean onInfo( MediaPlayer mp, int what, int extra ) {
	       Log.v( "HopAndroidMusicPlayer", "mediaplayer info: " +
		      what + " " + extra );
	       if( what == MediaPlayer.MEDIA_INFO_METADATA_UPDATE )
		  return true;
	       else
		  return false;
	    }
	 } );

      return mplayer;
   }
   
   // music player
   protected void server( InputStream ip, OutputStream op )
      throws IOException {
      switch( ip.read() ) {
	 case (byte)'x':
	    // exit
	    if( mplayer != null ) {
	       mplayer.release();
	       mplayer = null;
	       mplayerstate = MPLAYER_STATE_CLOSE;	       
	       return;
	    }

	 case (byte)'b':
	    // start
	    Log.v( "HopAndroidMusicPlayer", "mediaplayer start" );
	    if( mplayer != null ) {
	       mplayerstate = MPLAYER_STATE_PLAY;	       
	       mplayer.start();
	       return;
	    }

	 case (byte)'e':
	    // stop
	    Log.v( "HopAndroidMusicPlayer", "mediaplayer stop" );
	    if( mplayer != null ) {
	       mplayerstate = MPLAYER_STATE_STOP;	       
	       mplayer.stop();
	       return;
	    }

	 case (byte)'p':
	    // pause
	    Log.v( "HopAndroidMusicPlayer", "mediaplayer pause" );
	    if( mplayer != null ) {
	       mplayer.pause();
	       mplayerstate = MPLAYER_STATE_PAUSE;	       
	       return;
	    }

	 case (byte)'u':
	    // url
	    if( mplayer == null ) {
	       mplayer = make_mediaplayer();
	    } else {
	       mplayer.reset();
	    }
	    String uri = HopAndroid.read_string( ip );

	    File file = new File( uri );

	    if( file.exists() ) {
	       mplayer.setDataSource( activity, Uri.fromFile( file ) );
	       mplayer.prepare();
	       mplayer.start();
	       mplayerstate = MPLAYER_STATE_PLAY;	       
	    } else {
	       mplayer.setDataSource( uri );
	       mplayer.prepareAsync();
	       mplayerstate = MPLAYER_STATE_PLAY;	       
	    }
	    return;

	 case (byte)'v':
	    // set volume
	    if( mplayer == null ) {
	       mplayer = new MediaPlayer();
	    }

	    int voll = HopAndroid.read_int32( ip );
	    int volr = HopAndroid.read_int32( ip );

	    mplayer.setVolume( (float)voll/100, (float)volr/100 );
	    return;
	    
	 case (byte)'S':
	    // status: state, songlength, songpos
	    synchronized( op ) {
	       if( mplayer == null ) {
		  op.write( "(unspecified 0 0)".getBytes() );
	       } else {
		  if( mplayer.isPlaying() ) {
		     switch( mplayerstate ) {
			case MPLAYER_STATE_UNSPECIFIED:
			   op.write( "(unspecified ".getBytes() );
			   break;
			case MPLAYER_STATE_PLAY:
			   op.write( "(play ".getBytes() );
			   break;
			case MPLAYER_STATE_PAUSE:
			   op.write( "(pause ".getBytes() );
			   break;
			case MPLAYER_STATE_STOP:
			   op.write( "(stop ".getBytes() );
			   break;
			case MPLAYER_STATE_CLOSE:
			   op.write( "(close ".getBytes() );
			   break;
			default:
			   op.write( "(unspecified ".getBytes() );
			   break;
		     }
		     op.write( Integer.toString( mplayer.getDuration() / 1000 ).getBytes() );
		     op.write( " ".getBytes() );
		     op.write( Integer.toString( mplayer.getCurrentPosition() / 1000 ).getBytes() );
		     op.write( ")".getBytes() );
		  } else {
		     op.write( "(stop 0 0)".getBytes() );
		  }
	       }
	       op.flush();
	    }
	    
	    return;
      }
   }
}
