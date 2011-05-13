/*=====================================================================*/
/*    .../arch/android/src/fr/inria/hop/HopPluginMediaAudio.java       */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Wed May 11 08:47:25 2011                          */
/*    Last change :  Fri May 13 16:53:07 2011 (serrano)                */
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
      Genres._ID, // 0
      Genres.NAME, // 1
   };
   private static final String[] ARTIST_LOOKUP_PROJECTION = new String[] {
      Genres._ID, // 0
      Artists.ARTIST, // 1
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
	    
	 case (byte)'a':
	    // query artists by genre
	    queryGenreArtists( op, HopDroid.read_string( ip ) );
	    break;
      }
      op.flush();

      return;
   }

   private void queryGenres( OutputStream op ) throws IOException {
      ContentResolver cr = activity.getContentResolver();
      Cursor cur = cr.query( Genres.EXTERNAL_CONTENT_URI,
			     GENRE_LOOKUP_PROJECTION,
			     null,
			     null,
			     null );
      synchronized( op ) {
	 if( cur == null ) {
	    op.write( "()".getBytes() );
	 } else {
	    op.write( "(".getBytes() );
	    if( cur.moveToFirst() ) {
	       int i = cur.getColumnIndex( Genres.NAME ); 
	       do {
		  String genre = cur.getString( i );
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
      ContentResolver cr = activity.getContentResolver();
      Cursor cur = cr.query( Artists.EXTERNAL_CONTENT_URI,
			     ARTIST_LOOKUP_PROJECTION,
			     null, 
			     null,
			     null );
      synchronized( op ) {
	 if( cur == null ) {
	    op.write( "()".getBytes() );
	 } else {
	    op.write( "(".getBytes() );
	    if( cur.moveToFirst() ) {
	       int i = cur.getColumnIndex( Artists.ARTIST );
	       do {
		  String artist = cur.getString( 1 );
		  
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

   private Uri makeGenreUri( String id ) {
      return Uri.parse(
	 new StringBuilder()
	 .append( Genres.EXTERNAL_CONTENT_URI.toString() )
	 .append( "/" )
	 .append( id )
	 .append( "/" )
	 .append( Genres.Members.CONTENT_DIRECTORY )
	 .toString());
   }
   
   private void queryGenreArtists( OutputStream op, String genre )
      throws IOException {
      ContentResolver cr = activity.getContentResolver();
      Cursor cur = cr.query( Genres.EXTERNAL_CONTENT_URI,
			     GENRE_LOOKUP_PROJECTION,
			     Genres.NAME + "='" + genre + "'", 
			     null,
			     null );
      synchronized( op ) {
	 if( cur == null ) {
	    op.write( "()".getBytes() );
	 } else {
	    if( cur.moveToFirst() ) {
	       int i = cur.getColumnIndex( Genres._ID );
	       Log.d( "HopPluginMediaAudio", "id for \"" + genre + "\"=" + i );
	       Log.d( "HopPluginMediaAudio", "uri=" + makeGenreUri( cur.getString( i ) ) );
	       Cursor c = cr.query( makeGenreUri( cur.getString( i ) ),
				    new String[] { Media.ARTIST },
				    null,
				    null,
				    null );
	       cur.close();

	       Log.d( "HopPluginMediaAudio.java", "C.getCount=" + c.getCount() );
	       
	       if( c.getCount() == 0 ) {
		  op.write( "()".getBytes() );
	       } else {
		  op.write( "(".getBytes() );
		  c.moveToFirst();
		  int j = c.getColumnIndex( Media.ARTIST );
		  do {
		     String artist = c.getString( j );
		  
		     Log.d( "HopPluginMediaAudio.java", "artist=" + artist );
		     
		     op.write( "\"".getBytes() );
		     op.write( artist.getBytes() );
		     op.write( "\" ".getBytes() );
		  } while( c.moveToNext() );
		  op.write( ")".getBytes() );
	       }
	       
	       c.close();
	    }
	 }
      }
   }
}
