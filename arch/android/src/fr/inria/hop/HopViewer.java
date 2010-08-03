package fr.inria.hop;

import android.app.Activity;
import android.os.Bundle;
import android.util.Log;

public class HopViewer extends Activity {
    @Override
    public void onCreate(Bundle icicle) {
        super.onCreate(icicle);
        Log.v("HopViewer", "onCreate()");

        Log.v("HopViewer", "onCreate(): Hop()!");
        Hop hop= new Hop ();

        Log.v("HopViewer", "onCreate(): Hop.init()");
        hop.init (this);

        Log.v("HopViewer", "onCreate(): set layout");
        setContentView(R.layout.logcat);

        Log.v("HopViewer", "onCreate(): Hop.run()");
        hop.run ();
    }
}
