/*=====================================================================*/
/*    .../2.2.x/arch/android/src/fr/inria/hop/HopPluginSensor.java     */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Thu Oct 14 11:11:23 2010                          */
/*    Last change :  Tue Jan 11 18:02:23 2011 (serrano)                */
/*    Copyright   :  2010-11 Manuel Serrano                            */
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
   final int TYPE_ACCELEROMETER = 5;
   final int TYPE_PRESSURE = 6;
   final int[] SENSORTYPES = {
      Sensor.TYPE_ORIENTATION,
      Sensor.TYPE_LIGHT,
      Sensor.TYPE_MAGNETIC_FIELD,
      Sensor.TYPE_PROXIMITY,
      Sensor.TYPE_TEMPERATURE,
      Sensor.TYPE_ACCELEROMETER,
      Sensor.TYPE_PRESSURE,
   };

   // constructor
   HopPluginSensor( HopDroid h, Activity a, String n ) {
      super( h, a, n );
   }

   // kill
   public void kill() {
      super.kill();

      if( sensormanager != null ) {
	 for( int i = 0; i < TYPE_PRESSURE + 1; i++ ) {
	    if( listeners[ i ] != null ) {
	       sensormanager.unregisterListener( listeners[ i ] );
	    }
	 }
	 sensormanager = null;
      }
   }
   
   // instance variables
   SensorManager sensormanager;
   final SensorEventListener[] listeners =
      new SensorEventListener[ TYPE_PRESSURE + 1 ];
   final Object[] sensors =
      new Object[ TYPE_PRESSURE + 1 ];
   final boolean[] activelisteners =
      new boolean[ TYPE_PRESSURE + 1 ];
   final int[] delays =
      new int[ TYPE_PRESSURE + 1 ];
   final int[] counters =
      new int[ TYPE_PRESSURE + 1 ];
   final Object[] values =
      new Object[ TYPE_PRESSURE + 1 ];
   final int[] hoplisteners =
      new int[ TYPE_PRESSURE + 1 ];
      
   int ttl = 100;
   
   // utility
   private static String values_to_sexp( float[] v ) {
      return "(" + v[ 0 ] + " " + " " + v[ 1 ] + " " + v[ 2 ] + ")";
   }

   // create the sensor manager and get all the sensors
   private void init_sensormanager() {
      if( sensormanager == null ) {
	 sensormanager =
	    (SensorManager)activity.getSystemService( Context.SENSOR_SERVICE );
	 for( int i = 0; i < TYPE_PRESSURE + 1; i++ ) {
	    sensors[ i ] = sensormanager.getSensorList( SENSORTYPES[ i ] );
	 }
      }
   }

   // sensor_name
   private String sensor_name( int type ) {
      switch( type ) {
	 case Sensor.TYPE_ACCELEROMETER:
	    return "accelerometer";
	 case Sensor.TYPE_GYROSCOPE:
	    return "gyroscope";
	 case Sensor.TYPE_LIGHT:
	    return "light";
	 case Sensor.TYPE_MAGNETIC_FIELD:
	    return "magnetic-field";
	 case Sensor.TYPE_ORIENTATION:
	    return "orientation";
	 case Sensor.TYPE_PRESSURE:
	    return "pressure";
	 case Sensor.TYPE_PROXIMITY:
	    return "proximity";
	 case Sensor.TYPE_TEMPERATURE:
	    return "temperature";
	 default:
	    return "unknown";
      }
   }

   // start_listener
   private void start_listener( final InputStream ip,
				final OutputStream op,
				final int type,
				final int delay )
      throws IOException {
      final Sensor sensor = ((List<Sensor>)sensors[ type ]).get( 0 );
	    
      // add the listener if it is not bound yet
      synchronized( activelisteners ) {
	 if( listeners[ type ] == null ) {
	    values[ type ] = null;
	    counters[ type ] = -1;

	    Log.d( "HopDroidSensor", "installing sensor listener: " + type );
	    listeners[ type ] = new SensorEventListener() {
		  public void onSensorChanged( SensorEvent event ) {
		     boolean drop;
			   
		     synchronized( counters ) {
			drop = counters[ type ]++ > ttl;
		     }

		     if( drop && hoplisteners[ type ] == 0 ) {
			// we have dropped ten values, get rid of that listener
			Log.d( "HopDroidSensor", "dropping...sensor=" + event.sensor.getName()
			       + " type=" + type );
			synchronized( activelisteners ) {
			   if( activelisteners[ type ] ) {
			      activelisteners[ type ] = false;
			      Log.d( "HopDroidSensor", "unregister type=" + type );
			      sensormanager.unregisterListener( this );
			   }
			}
		     } else {
			synchronized( values ) {
			   values[ type ] = event.values;			      
			}
			if( hoplisteners[ type ] > 0 ) {
			   handroid.pushEvent(
			      sensor_name( SENSORTYPES[ type ] ),
			      values_to_sexp( (float [])values[ type ] ) );
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
		  sensormanager.unregisterListener( listeners[ type ] );
		  sensormanager.registerListener( listeners[ type ], sensor, delay );
	       }
	    }
	 }
      }

      // reset the counters for this type
      synchronized( counters ) {
	 counters[ type ] = 0;
      }
   }
      
   // sensor manager
   protected void server( final InputStream ip, final OutputStream op )
      throws IOException {
      
      switch( HopDroid.read_int( ip ) ) {
	 case (byte)'x':
	    // exit
	    for( int i = 0; i < TYPE_PRESSURE + 1; i++ ) {
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
	       for( int j = 0; j < TYPE_PRESSURE + 1; j++ ) {
		  List<Sensor> l = (List<Sensor>)sensors[ j ];

		  for( int i = 0 ; i < l.size() ; i++ ) {
		     Sensor s = l.get( i );
		     op.write( "(".getBytes() );
		     op.write( sensor_name( s.getType() ).getBytes() ); 
		     op.write( " \"".getBytes() );
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
	    }
	    break;

	 case (byte)'a':
	    // get sensors info
	    init_sensormanager();
	    
	    int t = HopDroid.read_int32( ip );
	    if( ((List<Sensor>)sensors[ t ]).size() > 0 ) {
	       hoplisteners[ t ]++;
	       start_listener( ip, op, t, SensorManager.SENSOR_DELAY_NORMAL );
	    }
	    break;
	    
	 case (byte)'r':
	    // get sensors info
	    init_sensormanager();

	    hoplisteners[ HopDroid.read_int32( ip ) ]--;
	    break;
	    
	 case (byte)'b':
	    init_sensormanager();
	    
	    final int type = HopDroid.read_int32( ip );
	    int delay = SensorManager.SENSOR_DELAY_NORMAL;
	    
	    ttl = HopDroid.read_int32( ip );
	    
	    switch( HopDroid.read_int32( ip ) ) {
	       case 0: delay = SensorManager.SENSOR_DELAY_NORMAL; break;
	       case 1: delay = SensorManager.SENSOR_DELAY_UI; break;
	       case 2: delay = SensorManager.SENSOR_DELAY_GAME; break;
	       case 3: delay = SensorManager.SENSOR_DELAY_FASTEST; break;
	    }

	    if( ((List<Sensor>)sensors[ type ]).size() > 0 ) {
	       start_listener( ip, op, type, delay );
	    
	       // check if we already have a value
	       synchronized( values ) {
		  synchronized( op ) {
		     if( values[ type ] != null ) {
			op.write( values_to_sexp( (float [])values[ type ] ).getBytes() );
		     } else {
			op.write( "#f".getBytes() );
		     }
		  }
	       }
	    } else {
	       synchronized( op ) {
		  op.write( "#unspecified".getBytes() );
	       }
	    }

	    return;
      }
   }
}

