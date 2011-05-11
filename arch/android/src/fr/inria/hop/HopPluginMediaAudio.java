/*=====================================================================*/
/*    .../arch/android/src/fr/inria/hop/HopPluginMediaAudio.java       */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Wed May 11 08:47:25 2011                          */
/*    Last change :  Wed May 11 10:31:40 2011 (serrano)                */
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
import MediaStore.Audio.Genres;

import java.net.*;
import java.io.*;

/*---------------------------------------------------------------------*/
/*    The class                                                        */
/*---------------------------------------------------------------------*/
public class HopPluginMediaAudio extends HopPlugin {
   // private fields
   private static final String[] GENRE_LOOKUP_PROJECTION = new String[] {
      Audio.Genres.NAME, // 0
   };
   private static final String[] ARTIST_LOOKUP_PROJECTION = new String[] {
      Audio.Artists.NAME, // 0
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
	    queryGenres( OutputStream op );
	    break;

	 case (byte)'A':
	    // query artists
	    queryArtists( OutputStream op );
	    break;
      }
      op.flush();

      return;
   }

   private void queryGenres( OutputStream op ) {
      Cursor cur = managedQuery( MediaStore.Audio.Genres.EXTERNAL_CONTENT_URI,
				 GENRE_LOOKUP_PROJECTION,
				 MediaStore.Audio.Genres.NAME + "=?",
				 null,
				 null );

      synchronized( op ) {
	 if( cursor == null ) {
	    op.write( "()".getBytes() );
	 } else {
	    op.write( "(".getBytes() );
	    
	    if( cursor.moveToFirst() ) {
	       do {
		  String genre = cursor.getString( 0 );
		     
		  op.write( "\"".getBytes() );
		  op.write( genre.getBytes() );
		  op.write( "\" ".getBytes() );
	       } while( cursor.moveToNext() );
	       
	       op.write( ")".getBytes() );
	    }
	    cursor.close();
	 }
      }
   }

   private void queryArtists( OutputStream op ) {
      Cursor cur = managedQuery( MediaStore.Audio.Artists.EXTERNAL_CONTENT_URI,
				 ARTIST_LOOKUP_PROJECTION,
				 MediaStore.Audio.Artists.NAME + "=?",
				 null,
				 null );

      synchronized( op ) {
	 if( cursor == null ) {
	    op.write( "()".getBytes() );
	 } else {
	    op.write( "(".getBytes() );
	    
	    if( cursor.moveToFirst() ) {
	       do {
		  String artist = cursor.getString( 0 );
		     
		  op.write( "\"".getBytes() );
		  op.write( artist.getBytes() );
		  op.write( "\" ".getBytes() );
	       } while( cursor.moveToNext() );
	       
	       op.write( ")".getBytes() );
	    }
	    cursor.close();
	 }
      }
   }
}
