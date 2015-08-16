/*=====================================================================*/
/*    .../hop/2.4.x/arch/android/src/fr/inria/hop/HopSettings.java     */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sat Nov  3 11:48:50 2012                          */
/*    Last change :  Thu Nov  8 13:24:29 2012 (serrano)                */
/*    Copyright   :  2012 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    Hop preferences                                                  */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    The package                                                      */
/*---------------------------------------------------------------------*/
package fr.inria.hop;

import android.app.*;
import android.os.*;
import android.util.Log;
import android.content.*;
import android.preference.*;
import android.view.*;
import android.view.View.*;
import android.widget.*;

import java.net.*;

/*---------------------------------------------------------------------*/
/*    The class                                                        */
/*---------------------------------------------------------------------*/
public class HopSettings extends PreferenceActivity {
    @Override
    public void onCreate( Bundle savedInstanceState ) {
       super.onCreate( savedInstanceState );
       addPreferencesFromResource( R.xml.preferences );
       
/*        setupHostname();                                             */

/*        Preference pport = (Preference)findPreference( "hop_port" ); */
/*        Log.d( "HopSettings", "pport=" + pport );                    */
/*        pport.setOnPreferenceChangeListener( new Preference.OnPreferenceChangeListener() { */
/* 	     public boolean onPreferenceChange( Preference preference, Object value ) { */
/* 		Log.d( "HopSettings", "port changed=" + (String)value ); */
/* 		return true;                                           */
/* 	     }                                                         */
/* 	  } );                                                         */
    }
   
/*    public void setupHostname() {                                    */
/*       TextView hostname = (TextView)this.findViewById( R.id.hostname ); */
/*       TextView hostip = (TextView)this.findViewById( R.id.hostip ); */
/*                                                                     */
/*       Log.d( "HopSettings", "hostname=" + hostname );               */
/*       Log.d( "HopSettings", "hostip=" + hostip );                   */
/*                                                                     */
/*       try {                                                         */
/* 	 InetAddress addr = java.net.InetAddress.getLocalHost();       */
/* 	 hostname.append( addr.getCanonicalHostName() );               */
/* 	 hostip.append( addr.getHostAddress() );                       */
/*       } catch( Exception _ ) {                                      */
/* 	 hostname.append( "" );                                        */
/* 	 hostip.append( "" );                                          */
/*       }                                                             */
/*    }                                                                */
}
