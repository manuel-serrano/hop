/*=====================================================================*/
/*    .../arch/android/src/fr/inria/hop/HopAndroidSensor.java          */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Thu Oct 14 11:11:23 2010                          */
/*    Last change :  Fri Oct 15 08:31:09 2010 (serrano)                */
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
   final SensorEventListener[] listeners =
      new SensorEventListener[ TYPE_TRICORDER + 1 ];
   final int[] activelisteners =
      new boolean[ TYPE_TRICORDER + 1 ];
   final int[] delays =
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

	 case (byte)'i':
	    // get sensors info
	    if( sensormanager == null ) {
	       sensormanager =
		  (SensorManager)activity.getSystemService( Context.SENSOR_SERVICE );
	    }
	    List<Sensor> sensors = sensorManager.getSensorList( Sensor.TYPE_ALL );
	    synchronized( op ) {
	       for( int i = 0 ; i < sensors.size() ; i++ ) {
		  Sensor s = sensors.get( i );
		  op.write( "(".getBytes() );
		  switch( sensor.getType() ) {
		     case TYPE_ACCELEROMETER: op.write( "accelerometer ".getBytes() ); break;
		     case TYPE_GYROSCOPE: op.write( "gyroscope ".getBytes() ); break;
		     case TYPE_LIGHT: op.write( "light ".getBytes() ); break;
		     case TYPE_MAGNETIC_FIELD: op.write( "magnetic-field ".getBytes() ); break;
		     case TYPE_ORIENTATION: op.write( "orientation ".getBytes() ); break;
		     case TYPE_PRESSURE: op.write( "pressure ".getBytes() ); break;
		     case TYPE_PROXIMITY: op.write( "proximity ".getBytes() ); break;
		     case TYPE_TEMPERATURE: op.write( "temperature ".getBytes() ); break;
		     default: op.write( "unknown ".getBytes() ); break;
		  }
		  op.write( "\"".getBytes() );
		  op.write( sensor.getName() );
		  op.write( "\" ".getBytes() );
		  op.write( sensor.getMaximumRange().toString().getBytes() );
		  op.write( " ".getBytes() );
		  op.write( sensor.getResolution().toString().getBytes() );
		  op.write( " ".getBytes() );
		  op.write( sensor.getPower().toString().getBytes() );
		  op.write( " ".getBytes() );
		  op.write( ")\n".getBytes() );
	       }
	       op.flush();
	    }
	    break;

	 case (byte)'b':
	    final int type = HopAndroid.read_int32( ip );
	    int delay = SensorManager.SENSOR_DELAY_NORMAL;
	    ttl = HopAndroid.read_int32( ip );
	    
	    switch( HopAndroid.read_int32( ip ) ) {
	       case 0: delay = SensorManager.SENSOR_DELAY_FASTEST; break;
	       case 1: delay = SensorManager.SENSOR_DELAY_GAME; break;
	       case 2: delay = SensorManager.SENSOR_DELAY_NORMAL; break;
	       case 3: delay = SensorManager.SENSOR_DELAY_UI; break;
	    }
	    
	    // create the global sensormanager
	    if( sensormanager == null ) {
	       sensormanager =
		  (SensorManager)activity.getSystemService( Context.SENSOR_SERVICE );
	    }

	    // add the listener if it is not bound yet
	    synchronized( listeners ) {
		  if( listeners[ type ] == null ) {
		     values[ type ] = null;
		     counters[ type ] = -1;

		     Log.d( "HopAndroidSensor", "installing sensor listener: " + type );
		     listeners[ type ] = new SensorEventListener() {
			   public void onSensorChanged( SensorEvent event ) {
			      boolean drop;
			   
			      synchronized( counters ) {
				 drop = counters[ type ]++ > ttl;
			      }

			      if( drop ) {
				 // we have dropped ten values, get rid of that listener
				 Log.d( "HopAndroidSensor", "dropping...sensor=" + sensor + " type=" + type );
				 synchronized( activelisteners ) {
				    if( activelisteners[ type ] ) {
				       activelisteners[ type ] = false;
				       sensormanager.unregisterListener( listeners[ type ] );
				    }
				 }
			      } else {
				 synchronized( values ) {
				    values[ type ] = event.values;			      
				 }
			      }
			   }
	 
			   public void onAccuracyChanged( Sensor sensor, int accuracy ) {
			   }
			};
		  }
	    }

	    // activate the listener now we have one
	    synchronized( activelisteners ) {
	       if( !activelisteners[ type ] ) {
		  activelisteners[ type ] = true;
		  sensormanager.registerListener( listeners[ type ], SENSORTYPES[ type ], delay );
	       } else {
		  synchronized( delays ) {
		     if( delays[ type ] != delay ) {
			delays[ type ] = delay;
			sensormanager.unregisterListener( listeners[ type ]);
			sensormanager.registerListener( listeners[ type ], SENSORTYPES[ type ], delay );
		     }
		  }
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

