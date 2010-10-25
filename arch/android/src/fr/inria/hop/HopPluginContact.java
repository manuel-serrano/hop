/*=====================================================================*/
/*    .../arch/android/src/fr/inria/hop/HopPluginContact.java          */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Mon Oct 25 09:26:00 2010                          */
/*    Last change :  Mon Oct 25 09:59:05 2010 (serrano)                */
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

import java.net.*;
import java.io.*;
import java.util.*;

/*---------------------------------------------------------------------*/
/*    The class                                                        */
/*---------------------------------------------------------------------*/
public class HopPluginContact extends HopPlugin {
   
   // constructor
   HopPluginContact( HopAndroid h, Activity a, String n ) {
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
   void getContactList( final OutputStream op ) {
      // Run query
      Uri uri = ContactsContract.Contacts.CONTENT_URI;
      Cursor cur = managedQuery( uri, null, null, null, null );

      if( cur.moveToFirst() ) {
        String name; 
        String phoneNumber; 
        int nameColumn = cur.getColumnIndex(People.NAME); 
        int phoneColumn = cur.getColumnIndex(People.NUMBER);
        String imagePath; 
    
        do {
            // Get the field values
            name = cur.getString(nameColumn);
            phoneNumber = cur.getString(phoneColumn);
           
            // Do something with the values. 
            ... 

        } while( cur.moveToNext() );
      } else {
	 op.write( "()".getBytes() );
      }
   }
}

