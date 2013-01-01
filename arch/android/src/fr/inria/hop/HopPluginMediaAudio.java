/*=====================================================================*/
/*    .../arch/android/src/fr/inria/hop/HopPluginMediaAudio.java       */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Wed May 11 08:47:25 2011                          */
/*    Last change :  Sun Dec 30 12:44:45 2012 (serrano)                */
/*    Copyright   :  2011-12 Manuel Serrano                            */
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
      Albums.ARTIST,
      Albums.ALBUM,
      Albums.ALBUM_ART,
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
   public HopPluginMediaAudio( HopDroid h, String n ) {
      super( h, n );
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
	    
	 case (byte)'g':
	    // query artists by genre
	    queryGenreArtists( op, HopDroid.read_string( ip ) );
	    break;
	    
	 case (byte)'a':
	    // query album songs
	    queryAlbum( op, HopDroid.read_string( ip ) );
	    break;
	    
	 case (byte)'d':
	    // query album by artist
	    queryArtistAlbum( op, HopDroid.read_string( ip ) );
	    break;
      }
      op.flush();

      return;
   }

   private static byte[] escapeBytes( String s ) {
      if( s == null ) {
	 return "-".getBytes();
      } else if( s.indexOf( '\\' ) > 0 ) {
	 return s.replace( "'", "\\'" ).getBytes();
      } else {
	 return s.getBytes();
      }
   }
   
   private void queryGenres( OutputStream op ) throws IOException {
      ContentResolver cr = hopdroid.service.getContentResolver();
      Cursor cur = cr.query( Genres.EXTERNAL_CONTENT_URI,
			     GENRE_LOOKUP_PROJECTION,
			     null,
			     null,
			     null );
      synchronized( op ) {
	 if( cur == null ) {
	    // Log.d( "HopPluginMediaAudio", "NO GENRE!" );
	    op.write( "()".getBytes() );
	 } else {
	    op.write( "(".getBytes() );
	    if( cur.moveToFirst() ) {
	       int i = cur.getColumnIndex( Genres.NAME ); 
	       do {
		  String genre = cur.getString( i );
		  
		  // Log.d( "HopPluginMediaAudio", "querying genre: [" + genre + "]");
		  op.write( "\"".getBytes() );
		  op.write( escapeBytes( genre ) );
		  op.write( "\" ".getBytes() );
	       } while( cur.moveToNext() );
	    } else {
	       // Log.d( "HopPluginMediaAudio", "NO FIRST!" );
	       op.write( "\"<all>\"".getBytes() );
	    }
	    op.write( ")".getBytes() );
	    cur.close();
	 }
      }
   }

   private void queryArtists( OutputStream op ) throws IOException {
      ContentResolver cr = hopdroid.service.getContentResolver();
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
		  // Log.d( "HopPluginMediaAudio", "querying artist: [" + cur.getString( i ) + "]");
		  op.write( "\"".getBytes() );
		  op.write( escapeBytes( cur.getString( i ) ) );
		  op.write( "\" ".getBytes() );
	       } while( cur.moveToNext() );
	       
	    }
	    op.write( ")".getBytes() );
	    cur.close();
	 }
      }
   }

   private void queryAlbum( OutputStream op, String album ) throws IOException {
      ContentResolver cr = hopdroid.service.getContentResolver();
      Cursor cur = cr.query( Media.EXTERNAL_CONTENT_URI,
			     MEDIA_LOOKUP_PROJECTION,
			     Media.ALBUM + "=?",
			     new String[] { album },
			     Media.DATA + " ASC" );
      // Log.d( "HopPluginMediaAudio", "querying album: " + album + "..." );
      synchronized( op ) {
	 if( cur == null ) {
	    op.write( "()".getBytes() );
	 } else {
	    op.write( "(".getBytes() );
	    if( cur.moveToFirst() ) {
	       int ip = cur.getColumnIndex( Media.DATA );
	       int it = cur.getColumnIndex( Media.TITLE );
	       int ti = cur.getColumnIndex( Media.DURATION );
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
		  op.write( "\" time: ".getBytes() );
		  op.write( Integer.toString( Integer.parseInt( cur.getString( ti ) ) / 1000 ).getBytes() );
		  op.write( ") ".getBytes() );
	       } while( cur.moveToNext() );
	       
	    }
	    op.write( ")".getBytes() );
	    cur.close();
	 }
      }
   }

   private void queryArtistAlbum( OutputStream op, String artist )
      throws IOException {
      // Log.d( "HopPluginMediaAudio", "querying ArtistAlbum: [" + artist + "]");
      ContentResolver cr = hopdroid.service.getContentResolver();

      // get the artist first
      Cursor acur = cr.query( Artists.EXTERNAL_CONTENT_URI,
			      ARTIST_LOOKUP_PROJECTION,
			      Artists.ARTIST + "=?",
			      new String[] { artist },
			      null );
      synchronized( op ) {
	 if( acur == null ) {
	    op.write( "()".getBytes() );
	 } else {
	    if( acur.moveToFirst() ) {
	       long artistid = acur.getLong( acur.getColumnIndex( Artists._ID ) );
	       Uri uri = Artists.Albums.getContentUri( "external", artistid );
	       Cursor cur = cr.query( uri,
				      ALBUM_LOOKUP_PROJECTION,
				      null,
				      null,
				      null );

	       // Log.d( "HopPluginMediaAudio", "queryArtistAlbum, cur=" + (cur == null ? "null" : cur.getCount()) + " artistid=" + artistid );
	       
	       if( cur == null ) {
		  op.write( "()".getBytes() );
	       } else {
		  int j = cur.getColumnIndex( Albums.ALBUM );
		  int a = cur.getColumnIndex( Albums.ALBUM_ART );
		  
		  op.write( "(".getBytes() );

		  if( cur.moveToFirst() ) {
		     do {
			// Log.d( "HopPluginMediaAudio", "queryArtistAlbum, ALBUM=" + cur.getString( j ) + " ART=" + cur.getString( a ) );
			op.write( "(album: \"".getBytes() );
			op.write( escapeBytes( cur.getString( j )) );
			op.write( "\"".getBytes() );

			if( cur.getString( a ) != null &&
			    !cur.getString( a ).equals( "-" ) ) {
			   op.write( " image: \"".getBytes() );
			   op.write( escapeBytes( cur.getString( a )) );
			   op.write( "\"".getBytes() );
			}
			op.write( ")".getBytes() );
		     } while( cur.moveToNext() );
		  }
		     
		  op.write( ")".getBytes() );
		  cur.close();
	       }
	    } else {
	       op.write( "()".getBytes() );
	    }

	    acur.close();
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
      // Log.d( "hopPluginMediaAudio", "queryGenreArists genre=\"" + genre + "\"" );
      if( genre.equals( "<all>" ) ) {
	 queryArtists( op );
      } else {
	 ContentResolver cr = hopdroid.service.getContentResolver();
	 Cursor cur = cr.query( Genres.EXTERNAL_CONTENT_URI,
				GENRE_LOOKUP_PROJECTION,
				Genres.NAME + "=?",
				new String[] { genre },
				null );
	 // Log.d( "hopPluginMediaAudio", "queryGenreArtist genre=\"" + genre + "\"" );
	 synchronized( op ) {
	    if( cur == null || cur.getCount() == 0 ) {
	       cur.close();
	       // Log.d( "hopPluginMediaAudio", "cur null.1" );
	       op.write( "()".getBytes() );
	       if( cur != null ) cur.close();
	    } else {
	       if( cur.moveToFirst() ) {
		  int i = cur.getColumnIndex( Genres._ID );
		  Cursor c = cr.query( makeGenreUri( cur.getString( i ) ),
				       new String[] { Media.ARTIST },
				       null,
				       null,
				       Media.ARTIST + " ASC" );
		  if( c != null ) {
		     String cartist = "";
		  
		     op.write( "(".getBytes() );
		     if( c.moveToFirst() ) {
			int j = c.getColumnIndex( Media.ARTIST );
			do {
			   String artist = c.getString( j );
		     
			   // Log.d( "hopPluginMediaAudio", "artist=" + artist );

			   if( !artist.equals( cartist ) ) {
			      op.write( "\"".getBytes() );
			      op.write( escapeBytes( artist ) );
			      op.write( "\" ".getBytes() );
			      cartist = artist;
			   }
			} while( c.moveToNext() );
		     }
		     op.write( ")".getBytes() );
		  }
	       
		  c.close();
	       }
	    
	       cur.close();
	    }
	 }
      }
   }
}
