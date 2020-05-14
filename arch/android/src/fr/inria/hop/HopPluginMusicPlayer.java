/*=====================================================================*/
/*    .../arch/android/src/fr/inria/hop/HopPluginMusicPlayer.java      */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Thu Oct 14 08:29:16 2010                          */
/*    Last change :  Wed Dec 28 07:51:43 2016 (serrano)                */
/*    Copyright   :  2010-16 Manuel Serrano                            */
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
import android.util.Base64;
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

   static final String DEVNULL = "/dev/null";

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

   private void resetSrc( MediaPlayer mp ) {
      try {
	 datasrc = DEVNULL;
	 mp.setDataSource( datasrc );
	 mp.prepare();
      } catch( Exception e ) {
	 ;
      }
   }
   
   // create a media player
   private MediaPlayer make_mediaplayer() {
      MediaPlayer player = new MediaPlayer();
      player.setWakeMode( hopdroid.service, PowerManager.PARTIAL_WAKE_LOCK );
      Log.v( "HopPluginMusicPlayer", "********** make mediaplayer..." );
      
      player.setOnPreparedListener( new MediaPlayer.OnPreparedListener() {
	    public void onPrepared( MediaPlayer mp ) {
	       mp.start();
	       Log.v( "HopPlugingMusicPlayer", "prepared..." );
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

      player.setOnErrorListener( new MediaPlayer.OnErrorListener() {
	    public boolean onError( MediaPlayer mp, int what, int extra ) {
	       if( datasrc != DEVNULL ) {
		  Log.v( "HopPluginMusicPlayer", "mediaplayer error: " +
			 what + " (" + strerror( what ) + ") " +
			 extra + " (" + strerror( extra ) + ") " +
			 ", error playing \"" + datasrc + "\"" );
		  mp.reset();
		  mp.release();
		  mplayer = null;
		  resetSrc( mp );

		  hopdroid.pushEvent( "androidmusic-error",
				      "\"error playing \\\"" + datasrc + "\\\"\"" );

	       } 
	       return false;
	    }
	 } );

      player.setOnInfoListener( new MediaPlayer.OnInfoListener() {
	    public boolean onInfo( MediaPlayer mp, int what, int extra ) {
	       Log.v( "HopPluginMusicPlayer", "mediaplayer info: " +
		      what + " " + extra );
	       if( what == MediaPlayer.MEDIA_INFO_METADATA_UPDATE )
		  return true;
	       else
		  return false;
	    }
	 } );

      player.setOnCompletionListener( new MediaPlayer.OnCompletionListener() {
	    public void onCompletion( MediaPlayer mp ) {
	       hopdroid.pushEvent( "androidmusic-state", "ended" );
	       ended = true;
	       resetSrc( mp );
	    }
	 } );

      player.setOnSeekCompleteListener( new MediaPlayer.OnSeekCompleteListener() {
	    public void onSeekComplete( MediaPlayer mp ) {
	       Log.v( "HopPluginMusicPlayer", "mediaplayer seek complete" );
	       ended = true;
	       resetSrc( mp);
	    }
	 } );

      return player;
   }

   // strerror
   private static String strerror( int extra ) {
      switch( extra ) {
	 case android.media.MediaPlayer.MEDIA_ERROR_UNKNOWN: return "MEDIA_ERROR_UNKNOWN";
	 case android.media.MediaPlayer.MEDIA_ERROR_SERVER_DIED: return "MEDIA_ERROR_SERVER_DIED";
	 case android.media.MediaPlayer.MEDIA_ERROR_IO: return "MEDIA_ERROR_IO";
	 case android.media.MediaPlayer.MEDIA_ERROR_MALFORMED: return "MEDIA_ERROR_MALFORMED";
	 case android.media.MediaPlayer.MEDIA_ERROR_UNSUPPORTED: return "MEDIA_ERROR_UNSUPPORTED";
	 case android.media.MediaPlayer.MEDIA_ERROR_TIMED_OUT: return "MEDIA_ERROR_TIMED_OUT";
	 case -2147483648: return "MEDIA_ERROR_SYSTEM";
	 default: return "???";
      }
   }

/*    static byte[] B( String s ) {                                    */
/*       return s.getBytes();                                          */
/*    }                                                                */
/*                                                                     */
/*    static String readLine( InputStream in, byte[] buf ) throws Exception { */
/*       int i = 0;                                                    */
/*       int s = 0;                                                    */
/*                                                                     */
/*       while( i < buf.length ) {                                     */
/* 	 buf[ i ] = (byte)in.read();                                   */
/*                                                                     */
/* 	 if( buf[ i ] == '\n' ) {                                      */
/* 	    if( s == 1 ) {                                             */
/* 	       return new String( buf, 0, i - 1 );                     */
/* 	    } else {                                                   */
/* 	       s = 0;                                                  */
/* 	    }                                                          */
/* 	 }                                                             */
/* 	                                                               */
/* 	 if( buf[ i ] == '\r' ) s = 1;                                 */
/* 	 i++;                                                          */
/*       }                                                             */
/*                                                                     */
/*       return null;                                                  */
/*    }                                                                */
/*                                                                     */
/*    static String readChars( InputStream in, byte[] buf, int z ) throws Exception { */
/*       int i = 0;                                                    */
/*       int s = 0;                                                    */
/*                                                                     */
/*       while( i < z ) {                                              */
/* 	 buf[ i ] = (byte)in.read();                                   */
/* 	 i++;                                                          */
/*       }                                                             */
/*                                                                     */
/*       return new String( buf, 0, z );                               */
/*    }                                                                */

   // flacStartOffset
   protected long flacStartOffset( String path ) {
      RandomAccessFile in = null;
      try {
	 in = new RandomAccessFile( path, "r" );
	 byte state = 0;
	 final byte[] buffer = new byte[ 1 ];
	 long o = 0;
      
	 while( true ) {
	    int sz = in.read( buffer, 0, 1 );
	    if( sz == 1 ) {
	       switch( state ) {
		  case 0:
		     if( buffer[ 0 ] == 'f' ) state++; break;
		  case 1:
		     if( buffer[ 0 ] == 'L' ) state++; else state = 0; break;
		  case 2:
		     if( buffer[ 0 ] == 'a' ) state++; else state = 0; break;
		  case 3:
		     if( buffer[ 0 ] == 'C' ) return o - 3; else state = 0;
	       }
	       o++;
	    } else {
	       return -1;
	    }
	 }
      } catch( Exception e ) {
	 return -1;
      } finally {
	 try {
	    if( in != null ) in.close();
	 } catch( Exception _e ) {
	    ;
	 }
      }
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
	       if( mplayer.isPlaying() ) {
		  mplayer.stop();
	       }
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
	       
	       if( datasrc.indexOf( ".flac" ) > 0 ) {
		  long offset = flacStartOffset( datasrc );
		  FileInputStream fin = new FileInputStream( file );
		  Log.v( "HopPluginMusicPlayer", "flac offset=" + offset );

		  try {
		     mplayer.setDataSource( fin.getFD(), offset, file.length() );
		  } finally {
		     fin.close();
		  }
	       } else {
		  mplayer.setDataSource( hopdroid.service, Uri.fromFile( file ) );
	       }

	       mplayer.prepare();
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
