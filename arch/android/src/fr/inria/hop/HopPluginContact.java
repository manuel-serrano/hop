/*=====================================================================*/
/*    .../arch/android/src/fr/inria/hop/HopPluginContact.java          */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Mon Oct 25 09:26:00 2010                          */
/*    Last change :  Tue Feb 18 15:24:46 2014 (serrano)                */
/*    Copyright   :  2010-14 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Accessing Contact database                                       */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    The package                                                      */
/*---------------------------------------------------------------------*/
package fr.inria.hop;

import android.app.*;
import android.content.*;
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
   protected void server( final InputStream ip, final OutputStream op )
      throws IOException {
      
      switch( HopDroid.read_int( ip ) ) {
	 case (byte)'l':
	    writeContactList( op );
	    break;
	    
	 case (byte)'r':
	    removeContact( op, ip );
	    break;
	    
	 case (byte)'a':
	    addContact( op, ip );
	    break;
       }
   }

   // writeContactList
   void writeContactList( final OutputStream op ) throws IOException {
      // Run query
      final String[] projection = new String[] {
	 ContactsContract.Contacts._ID,
	 ContactsContract.Contacts.DISPLAY_NAME
      };
      Uri uri = ContactsContract.Contacts.CONTENT_URI;
      ContentResolver cr = hopdroid.service.getContentResolver();
      Cursor cur = cr.query( uri, projection, null, null, null );

      if( cur.moveToFirst() ) {
	 op.write( "(".getBytes() );
	 do {
	    writeContact( op, cur );
	 } while( cur.moveToNext() );
	 op.write( ")".getBytes() );
      } else {
	 op.write( "()".getBytes() );
      }
   }

   // writeContact
   void writeContact( final OutputStream op, final Cursor cur )
      throws IOException {
      int id = cur.getInt( 0 );
      String name = cur.getString( 1 );

      op.write( "[".getBytes() );

      // name
      writeContactName( op, id, name );
      op.write( " ".getBytes() );

      // nicknames
      writeContactNicknames( op, id );
      op.write( " ".getBytes() );
      
      // organization
      writeContactOrganization( op, id );
      op.write( " ".getBytes() );

      // phones
      writeContactPhones( op, id );
      op.write( " " .getBytes() );

      // addresses
      writeContactAddresses( op, id );
      op.write( " " .getBytes() );

      // emails
      writeContactEmails( op, id );
      op.write( " ".getBytes() );

      // notes
      writeContactNotes( op, id );

      op.write( " ()".getBytes() );
      op.write( "]\n".getBytes() );
   }

   // writeContactName
   void writeContactName( final OutputStream op, int id, String name ) throws IOException {
      Cursor cur = getCursor(
	 id, 
	 new String[] {
	    StructuredName.GIVEN_NAME,
	    StructuredName.FAMILY_NAME,
	 },
	 StructuredName.CONTENT_ITEM_TYPE );

      if( cur.moveToFirst() ) {
	 writeOptionalString( op, cur, 0 );
	 op.write( " ".getBytes() );
	 writeOptionalString( op, cur, 1 );
      } else {
	 op.write( "\"".getBytes() );
	 op.write( name.getBytes() );
	 op.write( "\" \"\"".getBytes() );
      }
   }

   // writeContactPhones
   void writeContactPhones( final OutputStream op, int id ) throws IOException {
      Cursor cur = getCursor( 
	 id, 
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
   }

   // writeContactNicknames
   void writeContactNicknames( final OutputStream op, int id )
      throws IOException {
      Cursor cur = getCursor( 
	 id, 
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
   }

   // writeContactOrganization
   void writeContactOrganization( final OutputStream op, int id )
      throws IOException {
      Cursor cur = getCursor( 
	 id, 
	 new String[] {
	    Organization.COMPANY,
	 },
	 Organization.CONTENT_ITEM_TYPE );

      if( cur.moveToFirst() ) {
	 op.write( "\"".getBytes() );
	 op.write( cur.getString( 0 ).getBytes() );
	 op.write( "\"".getBytes() );
      } else {
	 op.write( "()".getBytes() );
      }
   }

   // writeContactAddresses
   void writeContactAddresses( final OutputStream op, int id ) throws IOException {
      Cursor cur = getCursor( 
	 id,
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
   }

   // writeContactEmails
   void writeContactEmails( final OutputStream op, int id ) throws IOException {
      Cursor cur = getCursor( 
	 id,
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
   }
      
   // writeContactNotes
   void writeContactNotes( final OutputStream op, int id ) throws IOException {
      Cursor cur = getCursor( 
	 id,
	 new String[] {
	    Website.URL,
	 },
	 Website.CONTENT_ITEM_TYPE );

      op.write( "(".getBytes() );
      
      // id
      op.write( "(android-id".getBytes() );
      op.write( " . \"".getBytes() );
      op.write( Integer.toString( id ).getBytes() );
      op.write( "\")".getBytes() );
      
      // website
      if( cur.moveToFirst() ) {
	 op.write( " (url".getBytes() );
	 op.write( " . \"".getBytes() );
	 op.write( cur.getString( 0 ).getBytes() );
	 op.write( "\")".getBytes() );
      }

      // note
      cur = getCursor( 
	 id,
	 new String[] {
	    Note.NOTE,
	 },
	 Note.CONTENT_ITEM_TYPE );

      if( cur.moveToFirst() ) {
	 op.write( " ".getBytes() );
	 op.write( cur.getString( 0 ).getBytes() );
      }
      
      op.write( ")".getBytes() );
   }
      
   // getCursor
   Cursor getCursor( int id, String[] projection, String mimetype ) throws IOException {
      ContentResolver cr = hopdroid.service.getContentResolver();
      Cursor cur = cr.query(
	 Data.CONTENT_URI, projection,
	 Data.CONTACT_ID + "=?" + " AND "
	 + Data.MIMETYPE + "='" + mimetype + "'",
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
   
   // writeOptionalString
   static void writeOptionalString( final OutputStream op, Cursor cur, int i )
      throws IOException {
      String s = cur.getString( i );
      if( s == null ) {
	 return ;
      } else {
	 op.write( "\"".getBytes() );
	 op.write( s.getBytes() );
	 op.write( "\"".getBytes() );
      }
   }

   // removeContact
   void removeContact( final OutputStream op, final InputStream ip )
      throws IOException {
      String id = HopDroid.read_string( ip );
      ContentResolver cr = hopdroid.service.getContentResolver();
      
      // remove from the sub-tables (MS 30oct2010: I'm not sure
      // this is useful. The Android documentation contains the following
      // warning: "Be careful with deleting Contacts! Deleting an aggregate
      // contact deletes all constituent raw contacts" but I'm not sure
      // what it exactly means.
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
   void addContact( final OutputStream op, final InputStream ip )
      throws IOException {
      ContentResolver cr = hopdroid.service.getContentResolver();
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

