/*=====================================================================*/
/*    .../arch/android/src/fr/inria/hop/HopPluginMediaAudio.java       */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Wed May 11 08:47:25 2011                          */
/*    Last change :  Wed May 11 16:41:23 2011 (serrano)                */
/*    Copyright   :  2011 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    Android Media Audio Plugin                                       */
/*    -------------------------------------------------------------    */
/*    This class gives access to the phone MediaAudio database.        */
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
import android.provider.MediaStore.Audio.*;
import android.database.Cursor;

import java.net.*;
import java.io.*;

/*---------------------------------------------------------------------*/
/*    The class                                                        */
/*---------------------------------------------------------------------*/
public class HopPluginMediaAudio extends HopPlugin {
   // private fields
   private static final String[] GENRE_LOOKUP_PROJECTION = new String[] {
      Genres.NAME, // 0
   };
   private static final String[] ARTIST_LOOKUP_PROJECTION = new String[] {
      Artists.ARTIST, // 0
   };
   
   // constructor
   public HopPluginMediaAudio( HopDroid h, Activity a, String n ) {
      super( h, a, n );
   }
   
   // plugin server
   protected void server( InputStream ip, OutputStream op )
      throws IOException {
      switch( HopDroid.read_int( ip ) ) {
	 case (byte)'G':
	    // query genres
	    queryGenres( op );
	    break;

	 case (byte)'A':
	    // query artists
	    queryArtists( op );
	    break;
      }
      op.flush();

      return;
   }

   private void queryGenres( OutputStream op ) throws IOException {
      Cursor cur = activity.managedQuery( Genres.EXTERNAL_CONTENT_URI,
					  GENRE_LOOKUP_PROJECTION,
					  null,
					  null,
					  null );

      Log.d( "HopDroidMediaAudio", "queryGenres"
	     + " " + (cur == null ? " (null)" : " (non-null)") );
      synchronized( op ) {
	 if( cur == null ) {
	    op.write( "()".getBytes() );
	 } else {
	    op.write( "(".getBytes() );
	    if( cur.moveToFirst() ) {
	       int i = cur.getColumnIndex( Genres.NAME ); 
	       Log.d( "HopDroidMediaAudio", "moveToFirst..." );
	       do {
		  Log.d( "HopDroidMediaAudio", "first to be displayed..." );
		  String genre = cur.getString( i );
		  Log.d( "HopDroidMediaAudio", "genre=" + genre );
		     
		  op.write( "\"".getBytes() );
		  op.write( genre.getBytes() );
		  op.write( "\" ".getBytes() );
	       } while( cur.moveToNext() );
	    }
	    cur.close();
	    op.write( ")".getBytes() );
	 }
      }
   }

   private void queryArtists( OutputStream op ) throws IOException {
      Cursor cur = activity.managedQuery( Artists.EXTERNAL_CONTENT_URI,
					  ARTIST_LOOKUP_PROJECTION,
					  Artists.ARTIST + "=?",
					  null,
					  null );

      Log.d( "HopDroidMediaAudio", "queryArtists" );
      synchronized( op ) {
	 if( cur == null ) {
	    op.write( "()".getBytes() );
	 } else {
	    op.write( "(".getBytes() );
	    if( cur.moveToFirst() ) {
	       do {
		  String artist = cur.getString( 0 );
		     
		  Log.d( "HopDroidMediaAudio", "artist=" + artist );
		  op.write( "\"".getBytes() );
		  op.write( artist.getBytes() );
		  op.write( "\" ".getBytes() );
	       } while( cur.moveToNext() );
	       
	    }
	    cur.close();
	    op.write( ")".getBytes() );
	 }
      }
   }
}
