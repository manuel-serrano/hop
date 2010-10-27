/*=====================================================================*/
/*    .../arch/android/src/fr/inria/hop/HopPluginCallLog.java          */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Mon Oct 25 09:26:00 2010                          */
/*    Last change :  Wed Oct 27 14:01:17 2010 (serrano)                */
/*    Copyright   :  2010 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    Accessing CallLog database                                       */
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
import android.provider.CallLog.Calls;

import java.net.*;
import java.io.*;
import java.util.*;

/*---------------------------------------------------------------------*/
/*    The class                                                        */
/*---------------------------------------------------------------------*/
public class HopPluginCallLog extends HopPlugin {
   
   // constructor
   HopPluginCallLog( HopDroid h, Activity a, String n ) {
      super( h, a, n );
   }

    // calllog manager
   protected void server( final InputStream ip, final OutputStream op )
      throws IOException {
      
       switch( ip.read() ) {
	 case (byte)'l':
	    int i = ip.read();
	    writeCallLogList( op, i );
	    break;
       }
   }

   // writeCallLogList
   void writeCallLogList( final OutputStream op, int i ) throws IOException {
      // Run query
      final String[] projection = new String[] {
	 Calls.TYPE,
	 Calls.NUMBER,
	 Calls.DATE,
	 Calls.DURATION,
	 Calls.CACHED_NAME,
      };
      Uri uri = Calls.CONTENT_URI;
      String order = Calls.DATE + " DESC";
      String limit = (i > 0) ? (order + " LIMIT " + i) : order;
      Cursor cur = activity.managedQuery( uri, projection, null, null, limit );

      if( cur.moveToFirst() ) {
	 op.write( "(".getBytes() );
	 do {
	    op.write( "(".getBytes() );
	    
	    // type
	    switch( cur.getInt( 0 ) ) {
	       case Calls.INCOMING_TYPE: 
		  op.write( "incoming".getBytes() );
		  break;
	       case Calls.MISSED_TYPE: 
		  op.write( "missed".getBytes() );
		  break;
	       case Calls.OUTGOING_TYPE: 
		  op.write( "missed".getBytes() );
		  break;
	       default:
		  op.write( "unknown".getBytes() );
		  break;
	    }
	    op.write( " ".getBytes() );
	    
	    // number
	    op.write( "\"".getBytes() );
	    op.write( cur.getString( 1 ).getBytes() );
	    op.write( "\" ".getBytes() );
	    
	    // date
	    op.write( cur.getString( 2 ).getBytes() );
	    op.write( " ".getBytes() );
	    
	    // duration
	    op.write( "#e".getBytes() );
	    op.write( cur.getString( 3 ).getBytes() );
	    op.write( " ".getBytes() );

	    // cached name
	    String cn = cur.getString( 4 );
	    byte[] cnb = (cn == null) ? "\"\"".getBytes() : cn.getBytes();
	    op.write( cnb );
	    op.write( ")".getBytes() );
	    
	 } while( cur.moveToNext() );
	 op.write( ")".getBytes() );
      } else {
	 op.write( "()".getBytes() );
      }
   }
}

