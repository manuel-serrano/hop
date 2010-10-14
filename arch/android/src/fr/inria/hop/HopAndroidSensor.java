/*=====================================================================*/
/*    .../arch/android/src/fr/inria/hop/HopAndroidSensor.java          */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Thu Oct 14 11:11:23 2010                          */
/*    Last change :  Thu Oct 14 18:40:12 2010 (serrano)                */
/*    Copyright   :  2010 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    Dealing with the sensors available on the phone.                 */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    The package                                                      */
/*---------------------------------------------------------------------*/
package fr.inria.hop;

import android.app.*;
import android.content.*;
import android.os.*;
import android.util.Log;
import android.hardware.*;

import java.net.*;
import java.io.*;

/*---------------------------------------------------------------------*/
/*    The class                                                        */
/*---------------------------------------------------------------------*/
public class HopAndroidSensor {
   // global constant
   final int TYPE_ORIENTATION = 0;
   final int TYPE_LIGHT = 1;
   final int TYPE_MAGNETICFIELD = 2;
   final int TYPE_PROXIMITY = 3;
   final int TYPE_TEMPERATURE = 4;
   final int TYPE_TRICORDER = 5;
   final int[] SENSORTYPES = {
      SensorManager.SENSOR_ORIENTATION,
      SensorManager.SENSOR_LIGHT,
      SensorManager.SENSOR_MAGNETIC_FIELD,
      SensorManager.SENSOR_PROXIMITY,
      SensorManager.SENSOR_TEMPERATURE,
      SensorManager.SENSOR_TRICORDER
   };
   
   // instance variables
   Activity activity;
   SensorManager sensormanager;
   final SensorListener[] listeners =
      new SensorListener[ TYPE_TRICORDER + 1 ];
   final boolean[] activelisteners =
      new boolean[ TYPE_TRICORDER + 1 ];
   final int[] counters =
      new int[ TYPE_TRICORDER + 1 ];
   final Object[] values =
      new Object[ TYPE_TRICORDER + 1 ];
   int ttl = 100;
   
   // constructor
   public HopAndroidSensor( Activity a ) {
      activity = a;
   }

   // utility
   private static byte[] values_to_sexp( float[] v ) {
      String buf = "(" + v[ 0 ] + " " + " " + v[ 1 ] + " " + v[ 2 ] + ")";
      return buf.getBytes();
   }
   // sensor manager
   protected void server( final InputStream ip, final OutputStream op )
      throws IOException {
      
      switch( ip.read() ) {
	 case (byte)'x':
	    // exit
	    for( int i = 0; i < TYPE_TRICORDER + 1; i++ ) {
	       if( listeners[ i ] != null ) {
		  sensormanager.unregisterListener( listeners[ i ] );
	       }
	    }
	    sensormanager = null;
	    return;

	 case (byte)'b':
	    final int type = HopAndroid.read_int32( ip );
	    ttl = HopAndroid.read_int32( ip );
	    
	    // create the global sensormanager
	    if( sensormanager == null ) {
	       sensormanager =
		  (SensorManager)activity.getSystemService( Context.SENSOR_SERVICE );
	    }

	    // add the listener if it does not exist
	    synchronized( listeners ) {
	       if( listeners[ type ] == null ) {
		  values[ type ] = null;
		  counters[ type ] = -1;

		  Log.d( "HopAndroidSensor", "installing sensor listener: " + type );
		  listeners[ type ] = new SensorListener() {
			public void onSensorChanged( int sensor, float[] v ) {
			   boolean drop;
			   
			   synchronized( counters ) {
			      drop = counters[ type ]++ > ttl;
			   }

			   if( drop ) {
			      // we have dropped ten values, get rid of that listener
			      Log.d( "HopAndroidSensor", "dropping...sensor=" + sensor + " type=" + type );
			      synchronized( activelisteners ) {
				 activelisteners[ type ] = false;
				 sensormanager.unregisterListener( listeners[ type ] );
			      }
			   } else {
			      synchronized( values ) {
				 values[ type ] = v;			      
			      }
			   }
			}
	 
			public void onAccuracyChanged( int sensor, int accuracy ) {
			}
		     };
	       }
	    }

	    // activate the listener now we have one
	    synchronized( activelisteners ) {
	       if( !activelisteners[ type ] ) {
		  activelisteners[ type ] = true;
		  sensormanager.registerListener( listeners[ type ], SENSORTYPES[ type ], SensorManager.SENSOR_DELAY_NORMAL );
	       }
	    }

	    // reset the counters for this type
	    synchronized( counters ) {
	       counters[ type ] = 0;
	    }
	    
	    // check if we already have a value
	    synchronized( values ) {
	       synchronized( op ) {
		  if( values[ type ] != null ) {
		     op.write( values_to_sexp( (float [])values[ type ] ) );
		  } else {
		     op.write( "#f ".getBytes() );
		  }
		  op.flush();
	       }
	    }

	    return;
      }
   }
}

