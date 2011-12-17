/*=====================================================================*/
/*    .../arch/android/src/fr/inria/hop/HopPluginMediaAudio.java       */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Wed May 11 08:47:25 2011                          */
/*    Last change :  Sat Dec 17 20:27:23 2011 (serrano)                */
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
      Genres._ID,
      Genres.NAME,
   };
   private static final String[] ARTIST_LOOKUP_PROJECTION = new String[] {
      Artists._ID,
      Artists.ARTIST,
   };
   private static final String[] ALBUM_LOOKUP_PROJECTION = new String[] {
      Albums._ID,
      Albums.ARTIST,
      Albums.ALBUM,
   };
   private static final String[] MEDIA_LOOKUP_PROJECTION = new String[] {
      Media._ID,
      Media.DATA,
      Media.TITLE,
      Media.MIME_TYPE,
      Media.DURATION,
      Media.TRACK,
      Media.ARTIST,
      Media.ALBUM,
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
	    Log.d( "HopPluginMediaAudio", "queryGenres" );
	    queryGenres( op );
	    break;

	 case (byte)'A':
	    // query artists
	    Log.d( "HopPluginMediaAudio", "queryArtists" );
	    queryArtists( op );
	    break;
	    
	 case (byte)'g':
	    // query artists by genre
	    Log.d( "HopPluginMediaAudio", "queryGenreArtists" );
	    queryGenreArtists( op, HopDroid.read_string( ip ) );
	    break;
	    
	 case (byte)'a':
	    // query album songs
	    Log.d( "HopPluginMediaAudio", "queryAlbum" );
	    queryAlbum( op, HopDroid.read_string( ip ) );
	    break;
	    
	 case (byte)'d':
	    // query album by artist
	    Log.d( "HopPluginMediaAudio", "queryArtistAlbum" );
	    queryArtistAlbum( op, HopDroid.read_string( ip ) );
	    break;
      }
      op.flush();

      return;
   }

   private static byte[] escapeBytes( String s ) {
      if( s.indexOf( '\\' ) > 0 ) {
	 return s.replace( "'", "\\'" ).getBytes();
      } else {
	 return s.getBytes();
      }
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
		  
		  Log.d( "HopPluginMediaAudio", "querying genre: [" + genre + "]");
		  op.write( "\"".getBytes() );
		  op.write( escapeBytes( genre ) );
		  op.write( "\" ".getBytes() );
	       } while( cur.moveToNext() );
	    }
	    op.write( ")".getBytes() );
	 }
      }
      cur.close();
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
		  Log.d( "HopPluginMediaAudio", "querying artist: [" + cur.getString( i ) + "]");
		  op.write( "\"".getBytes() );
		  op.write( escapeBytes( cur.getString( i ) ) );
		  op.write( "\" ".getBytes() );
	       } while( cur.moveToNext() );
	       
	    }
	    op.write( ")".getBytes() );
	 }
      }
      cur.close();
   }

   private void queryAlbum( OutputStream op, String album ) throws IOException {
      ContentResolver cr = activity.getContentResolver();
      Cursor cur = cr.query( Media.EXTERNAL_CONTENT_URI,
			     MEDIA_LOOKUP_PROJECTION,
			     Media.ALBUM + "=?",
			     new String[] { album },
			     null );
      Log.d( "HopPluginMediaAudio", "querying album: " + album + "..." );
      synchronized( op ) {
	 if( cur == null ) {
	    op.write( "()".getBytes() );
	 } else {
	    op.write( "(".getBytes() );
	    if( cur.moveToFirst() ) {
	       int ip = cur.getColumnIndex( Media.DATA );
	       int it = cur.getColumnIndex( Media.TITLE );
	       int id = cur.getColumnIndex( Media.DURATION );
	       int in = cur.getColumnIndex( Media.TRACK );
	       int ia = cur.getColumnIndex( Media.ARTIST );
	       int iv = cur.getColumnIndex( Media.ALBUM );
	       int im = cur.getColumnIndex( Media.MIME_TYPE );
	       do {
		  op.write( "(file: \"".getBytes() );
		  op.write( escapeBytes( cur.getString( ip ) ) );
		  op.write( "\" ".getBytes() );
		  op.write( "pos: ".getBytes() );
		  op.write( cur.getString( in ).getBytes() );
		  op.write( " ".getBytes() );
		  op.write( "id: ".getBytes() );
		  op.write( escapeBytes( cur.getString( in ) ) );
		  op.write( " ".getBytes() );
		  op.write( "mime-type: ".getBytes() );
		  op.write( escapeBytes( cur.getString( im ) ) );
		  op.write( " ".getBytes() );
		  op.write( "artist: \"".getBytes() );
		  op.write( escapeBytes( cur.getString( ia ) ) );
		  op.write( "\" ".getBytes() );
		  op.write( "title: \"".getBytes() );
		  op.write( escapeBytes( cur.getString( it ) ) );
		  op.write( "\" ".getBytes() );
		  op.write( "album: \"".getBytes() );
		  op.write( escapeBytes( cur.getString( iv ) ) );
		  op.write( "\") ".getBytes() );
	       } while( cur.moveToNext() );
	       
	    }
	    op.write( ")".getBytes() );
	 }
      }
      cur.close();
   }

   private void queryArtistAlbum( OutputStream op, String artist )
      throws IOException {
      Log.d( "HopPluginMediaAudio", "querying ArtistAlbum: [" + artist + "]");
      ContentResolver cr = activity.getContentResolver();
      Cursor cur = cr.query( Albums.EXTERNAL_CONTENT_URI,
			     ALBUM_LOOKUP_PROJECTION,
			     Albums.ARTIST + "=?", 
			     new String[] { artist },
			     null );
      Log.d( "HopPluginMediaAudio", "queryArtistAlbum, cur=" + (cur == null ? "null" : "pas-null") );
      synchronized( op ) {
	 Log.d( "HopPluginMediaAudio", "queryArtistAlbum, dans le block synchronized" );
	 if( cur == null ) {
	    op.write( "()".getBytes() );
	 } else {
	    if( cur.moveToFirst() ) {
	       int j = cur.getColumnIndex( Albums.ALBUM );
		  
	       Log.d( "HopPluginMediaAudio", "queryArtistAlbum, moved to first" );
	       op.write( "(".getBytes() );
		  
	       do {
		  Log.d( "HopPluginMediaAudio", "ALBUM=" + cur.getString( j ) );
		  op.write( "\"".getBytes() );
		  op.write( escapeBytes( cur.getString( j )) );
		  op.write( "\" ".getBytes() );
	       } while( cur.moveToNext() );
	       
	       op.write( ")".getBytes() );
	    } else {
	       op.write( "()".getBytes() );
	    }
	 }
      }
      
      cur.close();
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
			     Genres.NAME + "=?",
			     new String[] { genre },
			     null );
      Log.d( "hopPluginMediaAudio", "queryGenreArtist genre=\"" + genre + "\"" );
      synchronized( op ) {
	 if( cur == null ) {
	    cur.close();
	    Log.d( "hopPluginMediaAudio", "cur null.1" );
	    op.write( "()".getBytes() );
	 } else {
	    if( cur.moveToFirst() ) {
	       int i = cur.getColumnIndex( Genres._ID );
	       Cursor c = cr.query( makeGenreUri( cur.getString( i ) ),
				    new String[] { Media.ARTIST },
				    null,
				    null,
				    Media.ARTIST + " ASC" );
	       if( c.getCount() == 0 ) {
		  Log.d( "hopPluginMediaAudio", "count null.1" );
		  op.write( "()".getBytes() );
	       } else {
		  String cartist = "";
		  
		  op.write( "(".getBytes() );
		  c.moveToFirst();
		  int j = c.getColumnIndex( Media.ARTIST );
		  do {
		     String artist = c.getString( j );
		     
		     Log.d( "hopPluginMediaAudio", "artist=" + artist );

		     if( !artist.equals( cartist ) ) {
			op.write( "\"".getBytes() );
			op.write( escapeBytes( artist ) );
			op.write( "\" ".getBytes() );
			cartist = artist;
		     }
		  } while( c.moveToNext() );
		  op.write( ")".getBytes() );
	       }
	       
	       c.close();
	    }
	    
	    cur.close();
	 }
      }
   }
}
