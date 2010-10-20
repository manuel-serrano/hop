/*=====================================================================*/
/*    .../2.2.x/arch/android/src/fr/inria/hop/HopPluginSensor.java     */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Thu Oct 14 11:11:23 2010                          */
/*    Last change :  Tue Oct 19 19:07:38 2010 (serrano)                */
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
import java.util.*;

/*---------------------------------------------------------------------*/
/*    The class                                                        */
/*---------------------------------------------------------------------*/
public class HopPluginSensor extends HopPlugin {
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

   // constructor
   HopPluginSensor( HopAndroid h, Activity a, String n ) {
      super( h, a, n );
   }

   // instance variables
   SensorManager sensormanager;
   final SensorEventListener[] listeners =
      new SensorEventListener[ TYPE_TRICORDER + 1 ];
   final Object[] sensors =
      new Object[ TYPE_TRICORDER + 1 ];
   final boolean[] activelisteners =
      new boolean[ TYPE_TRICORDER + 1 ];
   final int[] delays =
      new int[ TYPE_TRICORDER + 1 ];
   final int[] counters =
      new int[ TYPE_TRICORDER + 1 ];
   final Object[] values =
      new Object[ TYPE_TRICORDER + 1 ];
   int ttl = 100;
   
   // utility
   private static byte[] values_to_sexp( float[] v ) {
      String buf = "(" + v[ 0 ] + " " + " " + v[ 1 ] + " " + v[ 2 ] + ")";
      return buf.getBytes();
   }

   // create the sensor manager and get all the sensors
   private void init_sensormanager() {
      if( sensormanager == null ) {
	 if( sensormanager == null ) {
	    sensormanager =
	       (SensorManager)activity.getSystemService( Context.SENSOR_SERVICE );
	 }
	 for( int i = 0; i < TYPE_TRICORDER; i++ ) {
	    sensors[ i ] = sensormanager.getSensorList( SENSORTYPES[ i ] );
	 }
      }
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
	    init_sensormanager();

	    synchronized( op ) {
	       op.write( "(".getBytes() );
	       for( int j = 0; j < TYPE_TRICORDER; j++ ) {
		  List<Sensor> l = (List<Sensor>)sensors[ j ];
		  for( int i = 0 ; i < l.size() ; i++ ) {
		     Sensor s = l.get( i );
		     op.write( "(".getBytes() );
		     switch( s.getType() ) {
			case Sensor.TYPE_ACCELEROMETER:
			   op.write( "accelerometer ".getBytes() ); break;
			case Sensor.TYPE_GYROSCOPE:
			   op.write( "gyroscope ".getBytes() ); break;
			case Sensor.TYPE_LIGHT:
			   op.write( "light ".getBytes() ); break;
			case Sensor.TYPE_MAGNETIC_FIELD:
			   op.write( "magnetic-field ".getBytes() ); break;
			case Sensor.TYPE_ORIENTATION:
			   op.write( "orientation ".getBytes() ); break;
			case Sensor.TYPE_PRESSURE:
			   op.write( "pressure ".getBytes() ); break;
			case Sensor.TYPE_PROXIMITY:
			   op.write( "proximity ".getBytes() ); break;
			case Sensor.TYPE_TEMPERATURE:
			   op.write( "temperature ".getBytes() ); break;
			default:
			   op.write( "unknown ".getBytes() );
		     }
		     op.write( "\"".getBytes() );
		     op.write( s.getName().getBytes() );
		     op.write( "\" ".getBytes() );
		     op.write( String.valueOf( s.getMaximumRange() ).getBytes() );
		     op.write( " ".getBytes() );
		     op.write( String.valueOf( s.getResolution() ).getBytes() );
		     op.write( " ".getBytes() );
		     op.write( String.valueOf( s.getPower() ).getBytes() );
		     op.write( " ".getBytes() );
		     op.write( ")\n".getBytes() );
		  }
	       }
	       op.write( ")".getBytes() );
	       op.flush();
	    }
	    break;

	 case (byte)'b':
	    init_sensormanager();
	    final int type = HopAndroid.read_int32( ip );
	    
	    if( ((List<Sensor>)sensors[ type ]).size() > 0 ) {
	       final Sensor sensor = ((List<Sensor>)sensors[ type ]).get( 0 );
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
				 Log.d( "HopAndroidSensor", "dropping...sensor=" + event.sensor.getName()
					+ " type=" + type );
				 synchronized( activelisteners ) {
				    if( activelisteners[ type ] ) {
				       activelisteners[ type ] = false;
				       sensormanager.unregisterListener( listeners[ type ], sensor );
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
		     sensormanager.registerListener( listeners[ type ], sensor, delay );
		  } else {
		     synchronized( delays ) {
			if( delays[ type ] != delay ) {
			   delays[ type ] = delay;
			   sensormanager.unregisterListener( listeners[ type ], sensor );
			   sensormanager.registerListener( listeners[ type ], sensor, delay );
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
	    }

	    return;
      }
   }
}

