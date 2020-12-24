/*=====================================================================*/
/*    .../hop/arch/android/src/fr/inria/hop/HopPluginContact.java      */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Mon Oct 25 09:26:00 2010                          */
/*    Last change :  Thu Dec 24 09:23:38 2020 (serrano)                */
/*    Copyright   :  2010-20 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Accessing Contact database                                       */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    The package                                                      */
/*---------------------------------------------------------------------*/
package fr.inria.hop;

import android.app.*;
import android.content.*;
import android.content.res.*;
import android.os.*;
import android.util.Log;
import android.database.Cursor;
import android.net.Uri;
import android.provider.ContactsContract;
import android.provider.ContactsContract.*;
import android.provider.ContactsContract.CommonDataKinds.Phone;
import android.provider.ContactsContract.CommonDataKinds.Email;
import android.provider.ContactsContract.CommonDataKinds.StructuredPostal;
import android.provider.ContactsContract.CommonDataKinds.StructuredName;
import android.provider.ContactsContract.CommonDataKinds.Nickname;
import android.provider.ContactsContract.CommonDataKinds.Organization;
import android.provider.ContactsContract.CommonDataKinds.Website;
import android.provider.ContactsContract.CommonDataKinds.Note;
import android.provider.Contacts;
import android.provider.Contacts.People;
import android.graphics.*;

import java.net.*;
import java.io.*;
import java.util.*;

/*---------------------------------------------------------------------*/
/*    The class                                                        */
/*---------------------------------------------------------------------*/
public class HopPluginContact extends HopPlugin {
   
   // constructor
   HopPluginContact( HopDroid h, String n ) {
      super( h, n );
   }

    // contact manager
   public void server( final InputStream ip, final OutputStream op )
      throws IOException {
      
      switch( HopDroid.read_int( ip ) ) {
	 case (byte)'l':
	    writeContactList( ip, op );
	    break;
	    
	 case (byte)'e':
	    writeContactEntry( ip, op );
	    break;
	    
	 case (byte)'r':
	    removeContact( ip, op );
	    break;
	    
	 case (byte)'a':
	    addContact( ip, op );
	    break;
       }
   }

   // writeContactList
   void writeContactList( final InputStream ip,
			  final OutputStream op ) throws IOException {
      String proj = HopDroid.read_string( ip );
      String sel = HopDroid.read_string( ip );
      boolean full = proj.equals( "full" );
      
      // Run query
      final String[] projection = new String[] {
	 ContactsContract.Contacts._ID,
	 ContactsContract.Contacts.LOOKUP_KEY,
	 ContactsContract.Contacts.DISPLAY_NAME,
	 ContactsContract.Contacts.HAS_PHONE_NUMBER
      };
      Uri uri = ContactsContract.Contacts.CONTENT_URI;
      String sort = ContactsContract.Contacts.DISPLAY_NAME + " ASC ";
      String selection = null;
      
      if( sel.equals( "phone" ) ) {
	 selection = "("
	    + ContactsContract.Contacts.IN_VISIBLE_GROUP
	    + " = '1'"
	    + " AND (" + ContactsContract.Contacts.HAS_PHONE_NUMBER + " != 0 )"
	    + " AND (" + ContactsContract.Contacts.DISPLAY_NAME + " != '' )"
	    + ")";
      }
      
      ContentResolver cr = hopdroid.activity.getContentResolver();
      Cursor cur = cr.query( uri,
			     projection,
			     selection,
			     null,
			     sort );

      if( cur.moveToFirst() ) {
	 op.write( "(".getBytes() );
	 do {
	    writeContact( cr, op, cur, full );
	 } while( cur.moveToNext() );
	 op.write( ")".getBytes() );
      } else {
	 op.write( "()".getBytes() );
      }
   }

   // writeContactEntry
   void writeContactEntry( final InputStream ip,
			   final OutputStream op ) throws IOException {
      final String[] projection = new String[] {
	 ContactsContract.Contacts._ID,
	 ContactsContract.Contacts.LOOKUP_KEY,
	 ContactsContract.Contacts.DISPLAY_NAME,
	 ContactsContract.Contacts.HAS_PHONE_NUMBER
      };
      Uri uri = ContactsContract.Contacts.CONTENT_URI;
      ContentResolver cr = hopdroid.activity.getContentResolver();
      String id = HopDroid.read_string( ip );
      String selection = "("
	 + ContactsContract.Contacts.LOOKUP_KEY + " = '" + id + "')";
      
      Cursor cur = cr.query( uri, projection, selection, null, null );

      Log.d( "HopPluginContact", "writeContactEntry id=" + id );
      
      if( cur.moveToFirst() ) {
	 writeContact( cr, op, cur, true );
      } else {
	 op.write( "#f".getBytes() );
      }
   }
   
   // writeContact
   void writeContact( ContentResolver cr, 
		      final OutputStream op,
		      final Cursor cur,
		      final boolean full )
      throws IOException {
      int id = cur.getInt( 0 );
      String key = cur.getString( 1 );
      String name = cur.getString( 2 );

      op.write( "#(".getBytes() );

      // id
      op.write( "\"".getBytes() );
      op.write( key.getBytes() );
      op.write( "\"".getBytes() );
      
      // name
      op.write( " ".getBytes() );
      writeContactName( cr, op, id, name );

      // thumbnail
      op.write( " ".getBytes() );
      writeContactThumbnail( cr, op, id, key );

      if( full ) {
	 // nicknames
	 op.write( " ".getBytes() );
	 writeContactNicknames( cr, op, id );
	 
	 // organization
	 op.write( " ".getBytes() );
	 writeContactOrganization( cr, op, id );

	 // phones
	 op.write( " " .getBytes() );
	 writeContactPhones( cr, op, id );

	 // addresses
	 op.write( " " .getBytes() );
	 writeContactAddresses( cr, op, id );

	 // emails
	 op.write( " ".getBytes() );
	 writeContactEmails( cr, op, id );

	 // photo
	 op.write( " ".getBytes() );
	 writeContactPhoto( cr, op, id );
	 
	 // url
	 op.write( " ".getBytes() );
	 writeContactUrl( cr, op, id );
	 
	 // notes
	 op.write( " ".getBytes() );
	 writeContactNotes( cr, op, id );
      }
      op.write( ")\n".getBytes() );
   }

   // writeContactName
   void writeContactName( ContentResolver cr, final OutputStream op, int id, String name ) throws IOException {
      Cursor cur = getCursor( cr, id, 
	 new String[] {
	    StructuredName.GIVEN_NAME,
	    StructuredName.FAMILY_NAME,
	 },
	 StructuredName.CONTENT_ITEM_TYPE );

      if( cur.moveToFirst() ) {
	 writeString( op, cur, 0 );
	 op.write( " ".getBytes() );
	 writeString( op, cur, 1 );
      } else {
	 op.write( "\"".getBytes() );
	 op.write( name.getBytes() );
	 op.write( "\" \"\"".getBytes() );
      }
      cur.close();
   }

   // writeContactPhones
   void writeContactPhones( ContentResolver cr, final OutputStream op, int id ) throws IOException {
      Cursor cur = getCursor( cr, id, 
	 new String[] {
	    Phone.LABEL,
	    Phone.NUMBER
	 },
	 Phone.CONTENT_ITEM_TYPE );

      if( cur.moveToFirst() ) {
	 op.write( "(".getBytes() );
	 do {
	    op.write( "[\"".getBytes() );
	    op.write( getBytes( cur, 0 ) );
	    op.write( "\" \"".getBytes() );
	    op.write( cur.getString( 1 ).getBytes() );
	    op.write( "\"]".getBytes() );
	 } while( cur.moveToNext() );
	 op.write( ")".getBytes() );
      } else {
	 op.write( "()".getBytes() );
      }
      cur.close();
   }

   // writeContactNicknames
   void writeContactNicknames( ContentResolver cr, final OutputStream op, int id )
      throws IOException {
      Cursor cur = getCursor( cr, id, 
	 new String[] {
	    Nickname.NAME,
	 },
	 Nickname.CONTENT_ITEM_TYPE );

      if( cur.moveToFirst() ) {
	 String s0 = cur.getString( 0 );
	 if( s0 == null ) {
	    op.write( "()".getBytes() );
	    return;
	 } else {
	    op.write( "(\"".getBytes() );
	    op.write( s0.getBytes() );
	    op.write( "\"".getBytes() );
	    while( cur.moveToNext() ) {
	       op.write( " \"".getBytes() );
	       op.write( cur.getString( 0 ).getBytes() );
	       op.write( "\"".getBytes() );
	    }
	    op.write( ")".getBytes() );
	 }
      } else {
	 op.write( "()".getBytes() );
      }
      cur.close();
   }

   // writeContactOrganization
   void writeContactOrganization( ContentResolver cr, final OutputStream op, int id )
      throws IOException {
      Cursor cur = getCursor( cr, id, 
	 new String[] {
	    Organization.COMPANY,
	 },
	 Organization.CONTENT_ITEM_TYPE );

      if( cur.moveToFirst() ) {
	 op.write( "\"".getBytes() );
	 op.write( cur.getString( 0 ).getBytes() );
	 op.write( "\"".getBytes() );
      } else {
	 op.write( "#f".getBytes() );
      }
      cur.close();
   }

   // writeContactAddresses
   void writeContactAddresses( ContentResolver cr, final OutputStream op, int id ) throws IOException {
      Cursor cur = getCursor( cr, id,
	 new String[] {
	    StructuredPostal.LABEL,
	    StructuredPostal.STREET,
	    StructuredPostal.POBOX,
	    StructuredPostal.CITY,
	    StructuredPostal.REGION,
	    StructuredPostal.POSTCODE,
	    StructuredPostal.COUNTRY
	 },
	 StructuredPostal.CONTENT_ITEM_TYPE );

      if( cur.moveToFirst() ) {
	 op.write( "(".getBytes() );
	 do {
	    op.write( "[\"".getBytes() );
	    op.write( getBytes( cur, 0, "home" ) );
	    op.write( "\" (".getBytes() );
	    writeOptionalString( op, cur, 1 );
	    op.write( " ".getBytes() );
	    writeOptionalString( op, cur, 2 );
	    op.write( " ".getBytes() );
	    writeOptionalString( op, cur, 3 );
	    op.write( " ".getBytes() );
	    writeOptionalString( op, cur, 4 );
	    op.write( " ".getBytes() );
	    writeOptionalString( op, cur, 5 );
	    op.write( ")]".getBytes() );
	 } while( cur.moveToNext() );
	 op.write( ")".getBytes() );
      } else {
	 op.write( "()".getBytes() );
      }
      cur.close();
   }

   // writeContactEmails
   void writeContactEmails( ContentResolver cr, final OutputStream op, int id ) throws IOException {
      Cursor cur = getCursor( cr, id,
	 new String[] {
	    Email.DATA,
	 },
	 Email.CONTENT_ITEM_TYPE );

      if( cur.moveToFirst() ) {
	 op.write( "(\"".getBytes() );
	 op.write( cur.getString( 0 ).getBytes() );
	 op.write( "\"".getBytes() );
	 while( cur.moveToNext() ) {
	    op.write( " \"".getBytes() );
	    op.write( cur.getString( 0 ).getBytes() );
	    op.write( "\"".getBytes() );
	 } 
	 op.write( ")".getBytes() );
      } else {
	 op.write( "()".getBytes() );
      }
      cur.close();
   }
      
   // writeContactPhoto
   void writeContactPhoto( ContentResolver cr, final OutputStream op, int id ) throws IOException {
      Cursor cur = getCursor( cr, id,
	 new String[] {
	    ContactsContract.Contacts.PHOTO_URI
	 },
	 null );

      Log.d( "HopPluginContact", ">>> photo.1 cur=" + id + (cur == null ? " null" : " pas null" ) );
      
      if( cur.moveToFirst() && cur.getString( 0 ) != null ) {
	 op.write( "\"".getBytes() );
	 op.write( cur.getString( 0 ).getBytes() );
	 op.write( "\"".getBytes() );
      } else {
	 op.write( "#f".getBytes() );
      }
      cur.close();
   }
      
   // writeContactUrl
   void writeContactUrl( ContentResolver cr, final OutputStream op, int id ) throws IOException {
      Cursor cur = getCursor( cr, id,
	 new String[] { Website.URL },
	 Website.CONTENT_ITEM_TYPE );

      if( cur.moveToFirst() ) {
	 op.write( "\"".getBytes() );
	 op.write( cur.getString( 0 ).getBytes() );
	 op.write( "\"".getBytes() );
      } else {
	 op.write( "#f".getBytes() );
      }
   }
      
   // writeContactThumbnail
   void writeContactThumbnail( ContentResolver cr, final OutputStream op, int id, String key ) throws IOException {
       Cursor cur = cr.query(
	  ContactsContract.Contacts.CONTENT_URI,
	  new String[] {
	    ContactsContract.Contacts._ID,
	    ContactsContract.Contacts.PHOTO_FILE_ID,
	    ContactsContract.Contacts.PHOTO_ID,
	    ContactsContract.Contacts.PHOTO_THUMBNAIL_URI,
	    ContactsContract.Contacts.PHOTO_URI
	  },
	  "(" + ContactsContract.Contacts.LOOKUP_KEY + " = '" + key + "')",
	  null, null );

      if( cur.moveToFirst() ) {
	 String thumb = cur.getString( 3 );
	 
	 if( thumb == null ) thumb = cur.getString( 4 );

	 if( thumb != null ) {
	    Log.d( "HopPluginContact", "thumb=" + thumb );
	 
	    try {
	       Uri uri;
	       AssetFileDescriptor afd = null;
	       
	       if( Build.VERSION.SDK_INT >= Build.VERSION_CODES.HONEYCOMB ) {
		  uri = Uri.parse( thumb );
	       } else {
		  final Uri curi = Uri.withAppendedPath(
		     ContactsContract.Contacts.CONTENT_URI, thumb );
		  uri = Uri.withAppendedPath(
                                curi,
				ContactsContract.Contacts.Photo.CONTENT_DIRECTORY );
	       }

	       afd = cr.openAssetFileDescriptor( uri, "r" );
	       FileInputStream in = afd.createInputStream();
	       ByteArrayOutputStream result = new ByteArrayOutputStream();
	       byte[] buffer = new byte[ 1024 ];
	       int length;
	       
	       while( (length = in.read( buffer )) != -1) {
		  result.write( buffer, 0, length );
	       }
	       
	       byte[] encoded = Base64.getEncoder().encode( result.toByteArray() );

	       op.write( "\"".getBytes() );
	       op.write( encoded );
	       op.write( "\"".getBytes() );

	       in.close();
	       afd.close();
	       
	       return;
	       
/* 	       if( fileDescriptor != null ) {                          */
/* 		  return BitmapFactory.decodeFileDescriptor( fd, null, null ); */
/* 	       }                                                       */
/*                                                                     */
/*                                                                     */
/*                                                                     */
/* 	       InputStream in = new java.net.URL( uri ).openStream();  */
/* 	       Bitmap bmp = bmp = BitmapFactory.decodeStream( in );    */
	    } catch (Exception e) {
	       Log.e("Error", e.getMessage());
	       e.printStackTrace();
	    }
	 }
      }
       
       op.write( "#unspecified".getBytes() );
   }
/*    void writeContactThumbnail( ContentResolver cr, final OutputStream op, int id ) throws IOException { */
/*       Uri contactUri = ContentUris.withAppendedId( Contacts.CONTENT_URI, id ); */
/*       Uri photoUri = Uri.withAppendedPath( contactUri, Contacts.Photo.CONTENT_DIRECTORY ); */
/*       Cursor cursor = cr().query(                                   */
/* 	 photoUri,                                                     */
/* 	 new String[] { Contacts.Photo.PHOTO }, null, null, null );    */
/*                                                                     */
/*       if( cursor == null ) {                                        */
/*          return null;                                               */
/*       }                                                             */
/*                                                                     */
/*       try {                                                         */
/*          if( cursor.moveToFirst() ) {                               */
/* 	    byte[] data = cursor.getBlob(0);                           */
/*              if( data != null ) {                                   */
/* 		Bitmap bmp = null;                                     */
/* 		ByteArrayOutputStream stream = new ByteArrayOutputStream(); */
/* 		bmp.compress( Bitmap.CompressFormat.JPEG, 100, stream ); */
/* byte[] byteArray = stream.toByteArray();                            */
/* 		return new ByteArrayInputStream(data);                 */
/*              }                                                      */
/*          }                                                          */
/*       } finally {                                                   */
/*          cursor.close();                                            */
/*       }                                                             */
/*       return null;                                                  */
/*    }                                                                */
/*                                                                     */
/*       Cursor cur = getCursor( cr, id,                               */
/* 	 new String[] {                                                */
/* 	    Email.DATA,                                                */
/* 	 },                                                            */
/* 	 Photo.2CONTENT_ITEM_TYPE );                                   */
/*                                                                     */
/*       if( cur.moveToFirst() ) {                                     */
/* 	 op.write( "(\"".getBytes() );                                 */
/* 	 op.write( cur.getString( 0 ).getBytes() );                    */
/* 	 op.write( "\"".getBytes() );                                  */
/* 	 while( cur.moveToNext() ) {                                   */
/* 	    op.write( " \"".getBytes() );                              */
/* 	    op.write( cur.getString( 0 ).getBytes() );                 */
/* 	    op.write( "\"".getBytes() );                               */
/* 	 }                                                             */
/* 	 op.write( ")".getBytes() );                                   */
/*       } else {                                                      */
/* 	 op.write( "()".getBytes() );                                  */
/*       }                                                             */
/*       cur.close();                                                  */
/*    }                                                                */
/*                                                                     */
   // writeContactNotes
   void writeContactNotes( ContentResolver cr, final OutputStream op, int id ) throws IOException {
      Cursor cur = getCursor( cr, id,
	 new String[] { Website.URL },
	 Website.CONTENT_ITEM_TYPE );

      op.write( "(".getBytes() );
      
      // id
      op.write( "(android-id . \"".getBytes() );
      op.write( Integer.toString( id ).getBytes() );
      op.write( "\")".getBytes() );
      
      // note
      cur = getCursor( cr, id,
	 new String[] { Note.NOTE },
	 Note.CONTENT_ITEM_TYPE );

      if( cur.moveToFirst() && cur.getString( 0 ) != null ) {
	 op.write( " ".getBytes() );
	 op.write( " (note . \"".getBytes() );
	 op.write( cur.getString( 0 ).getBytes() );
	 op.write( "\")".getBytes() );
      }

      op.write( ")".getBytes() );
      cur.close();
   }
      
   // getCursor
   Cursor getCursor( ContentResolver cr, int id, String[] projection, String mimetype ) throws IOException {
      Cursor cur = cr.query(
	 Data.CONTENT_URI, projection,
	 Data.CONTACT_ID + "=?" +
	 (mimetype != null ? " AND " + Data.MIMETYPE + "='" + mimetype + "'" : ""),
	 new String[] {
	    String.valueOf( id )
	 },
	 null );

      return cur;
   }

   // getBytes
   static byte[] getBytes( Cursor cur, int i ) {
      String s = cur.getString( i );
      if( s == null ) {
	 return "default".getBytes();
      } else {
	 return s.getBytes();
      }
   }
   
   // getBytes
   static byte[] getBytes( Cursor cur, int i, String def ) {
      String s = cur.getString( i );
      if( s == null ) {
	 return def.getBytes();
      } else {
	 return s.getBytes();
      }
   }
   
   // writeString
   static void writeString( final OutputStream op, Cursor cur, int i )
      throws IOException {
      String s = cur.getString( i );
      if( s == null ) {
	 op.write( "\"\"".getBytes() );
      } else {
	 op.write( "\"".getBytes() );
	 op.write( s.getBytes() );
	 op.write( "\"".getBytes() );
      }
   }

   // writeOptionalString
   static void writeOptionalString( final OutputStream op, Cursor cur, int i )
      throws IOException {
      String s = cur.getString( i );
      if( s == null ) {
	 return;
      } else {
	 op.write( "\"".getBytes() );
	 op.write( s.getBytes() );
	 op.write( "\"".getBytes() );
      }
   }

   // removeContact
   void removeContact( final InputStream ip, final OutputStream op )
      throws IOException {
      String id = HopDroid.read_string( ip );
      ContentResolver cr = hopdroid.activity.getContentResolver();
      
      // remove from the sub-tables (MS 30oct2010: I'm not sure
      // this is useful. The Android documentation contains the following
      // warning: "Be careful with deleting Contacts! Deleting an aggregate
      // contact deletes all constituent raw contacts" but I'm not sure
      // what it exactly means).
      removeCursor( cr, Nickname.CONTENT_ITEM_TYPE, id );
      removeCursor( cr, Organization.CONTENT_ITEM_TYPE, id );
      removeCursor( cr, Phone.CONTENT_ITEM_TYPE, id );
      removeCursor( cr, StructuredPostal.CONTENT_ITEM_TYPE, id );
      removeCursor( cr, Email.CONTENT_ITEM_TYPE, id );
      removeCursor( cr, Website.CONTENT_ITEM_TYPE, id );
      removeCursor( cr, Note.CONTENT_ITEM_TYPE, id );

      cr.delete(
	 Uri.withAppendedPath( ContactsContract.Contacts.CONTENT_URI, id ),
	 null, null );

      op.write( "#t ".getBytes() );
      return;
   }
	 
   // removeCursor
   static void removeCursor( ContentResolver cr, String mimetype, String id ) {
      cr.delete( 
	 Data.CONTENT_URI, 
	 Data.CONTACT_ID + "=?" + " AND "
	 + Data.MIMETYPE + "='" + mimetype + "'",
	 null );
   }

   // addContact
   void addContact( final InputStream ip, final OutputStream op )
      throws IOException {
      ContentResolver cr = hopdroid.activity.getContentResolver();
      ContentValues values = new ContentValues();
      String first = HopDroid.read_string( ip );
      String family = HopDroid.read_string( ip );
      String org = HopDroid.read_string( ip );
      String url = HopDroid.read_string( ip );
      String[] emails = HopDroid.read_stringv( ip );

      // bind the new person
      values.put( ContactsContract.Contacts.DISPLAY_NAME, first + " " + family );
      Uri uri = cr.insert( People.CONTENT_URI, values );

      // the emails
      Uri emailUri = Uri.withAppendedPath( uri, People.ContactMethods.CONTENT_DIRECTORY );
      values.clear();

      for( int i = 0; i < emails.length; i++ ) {
	 values.put( People.ContactMethods.KIND, Contacts.KIND_EMAIL );
	 values.put( People.ContactMethods.DATA, emails[ i ] );
	 values.put( People.ContactMethods.TYPE, People.ContactMethods.TYPE_HOME );
      }
      cr.insert( emailUri, values );   
      
      return;
   }
}

