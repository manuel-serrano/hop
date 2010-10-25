/*=====================================================================*/
/*    .../arch/android/src/fr/inria/hop/HopPluginContact.java          */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Mon Oct 25 09:26:00 2010                          */
/*    Last change :  Mon Oct 25 10:53:37 2010 (serrano)                */
/*    Copyright   :  2010 Manuel Serrano                               */
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

import java.net.*;
import java.io.*;
import java.util.*;

/*---------------------------------------------------------------------*/
/*    The class                                                        */
/*---------------------------------------------------------------------*/
public class HopPluginContact extends HopPlugin {
   
   // constructor
   HopPluginContact( HopDroid h, Activity a, String n ) {
      super( h, a, n );
   }

    // sensor manager
   protected void server( final InputStream ip, final OutputStream op )
      throws IOException {
      
       switch( ip.read() ) {
	 case (byte)'l':
	    getContactList( op );
       }
   }

   // getContactList
   void getContactList( final OutputStream op ) throws IOException {
      // Run query
      Uri uri = ContactsContract.Contacts.CONTENT_URI;
      Cursor cur = activity.managedQuery( uri, null, null, null, null );

      if( cur.moveToFirst() ) {
	 op.write( "(".getBytes() );
        do {
	   op.write( "(".getBytes() );
	   for( int i = 0; i < cur.getColumnCount(); i++ ) {
	      op.write( "(\"".getBytes() );
	      op.write( cur.getColumnName( i ).getBytes() );
	      op.write( "\" \"".getBytes() );
	      op.write( cur.getString( i ).getBytes() );
	      op.write( "\")".getBytes() );
	   }
	   op.write( ")\n".getBytes() );
        } while( cur.moveToNext() );
	 op.write( ")".getBytes() );
      } else {
	 op.write( "()".getBytes() );
      }
   }
}

