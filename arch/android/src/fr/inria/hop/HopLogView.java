package fr.inria.hop;

import java.lang.String;
import java.util.ArrayList;
import java.net.URL;
import java.net.HttpURLConnection;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.net.ProtocolException;
import java.util.concurrent.ArrayBlockingQueue;
import java.net.MalformedURLException;
import java.io.IOException;

import android.widget.TextView;
import android.widget.ScrollView;
import android.os.Handler;
import android.os.Message;
import android.content.Context;
import android.util.AttributeSet;

// Some code from Android Term app
// http://android.git.kernel.org/?p=platform/development.git;a=blob;f=apps/Term/src/com/android/term/Term.java;hb=HEAD
// Copyright (C) 2007 The Android Open Source Project
// Licensed under the Apache License, Version 2.0 (the "License")
public class HopLogView extends TextView {
    /**
     * We defer some initialization until we have been layed out in the view
     * hierarchy. The boolean tracks when we know what our size is.
     */
    private boolean mKnownSize;

    /**
     * Our private message id, which we use to receive new input from the
     * remote process.
     */
    private static final int UPDATE = 1;

    /**
     * Thread that polls for input from the remote process
     */

    private Thread mPollingThread;

    private ArrayBlockingQueue<String> mStringQueue;
    private BufferedReader rd;

    /**
     * Used to temporarily hold data received from the remote process. Allocated
     * once and used permanently to minimize heap thrashing.
     */
    // private byte[] mReceiveBuffer;

    /**
     * Our message handler class. Implements a periodic callback.
     */
    private final Handler mHandler = new Handler() {
        /**
         * Handle the callback message. Call our enclosing class's update
         * method.
         *
         * @param msg The callback message.
         */
        @Override
        public void handleMessage(Message msg) {
            if (msg.what == UPDATE) {
                update();
            }
        }
    };

    private HttpURLConnection logcat;

    private ScrollView mScroll;

    public HopLogView (Context context, AttributeSet attrs) {
        super (context, attrs);
        Hop.Log("HopLogView(Context, AttrSet)");
        this.init ();
    }

    public HopLogView (Context context, AttributeSet attrs, int defStyle) {
        super (context, attrs, defStyle);
        Hop.Log("HopLogView(Context, AttrSet, int)");
        this.init ();
    }

    public HopLogView (Context context) {
        super (context);
        Hop.Log("HopLogView(Context)");
        this.init ();
    }

    protected void init () {
        Hop.Log("HopLogView.init()");
        mStringQueue = new ArrayBlockingQueue<String>(10);
        rd= null;
    }

    protected void onSizeChanged(int w, int h, int oldw, int oldh) {
        super.onSizeChanged (w, h, oldw, oldh);

        // Hop.Log("onSizeChanged()" + w + ":" + h);
        if (!mKnownSize) {
            Hop.Log("onSizeChanged(): init");
            mKnownSize = true;

            // Set up a thread to read input from the socket
            // TODO: make it a proper class!
            mPollingThread = new Thread(new Runnable() {

                public void run() {
                    Hop.Log("mPollingThread.run()");

                    while (true) {
                        try {
                           // Hop.Log("connecting...");
                           logcat= (HttpURLConnection) new URL ("http://localhost:8080/logcat?"+lastId).openConnection ();
                           logcat.connect ();
                           // Hop.Log("connected to logcat!");
                           rd= new BufferedReader (new InputStreamReader (logcat.getInputStream()), 8196);

                           String line= rd.readLine ();
                           // Hop.Log("read: "+line);
                           while (line!=null) {
                              processLog (line);

                              line= rd.readLine ();
                              // Hop.Log("read: "+line);
                           }

                        } catch (MalformedURLException e) {
                           Hop.Log("HopLogView():" + e);
                           // TODO: FAIL
                        } catch (IOException e) {
                           Hop.Log("HopLogView():" + e);
                           // TODO: FAIL

                        } finally {
                           try {
                              // polling...
                              Thread.sleep (5000);
                           } catch (InterruptedException e) {
                              Hop.Log("HopLogView(): sleep() interrupted:" + e);
                           }
                        }
                    }
                }

               public void processLog (String line) {
                  // line comes in 3 models:
                  // ((## line
                  // ) (## line
                  // ))

                  // String.substring behaves just like Python's string slicing
                  // and don't try to use ==, use dead-brained .equals()
                  if (line.substring (0, 2).equals ("((")) {
                     line= line.substring (2);
                  } else if (line.substring (0, 2).equals (") ")) {
                     line= line.substring (3);
                  } else if (line.substring (0, 2).equals ("))")) {
                     line= null;
                  }

                  if (line!=null) {
                     // now process the ## and update lastId
                     String[] data= line.split (" ", 2);
                     try {
                        lastId= new Integer (data[0]);
                        // Hop.Log("lastId got up to: "+lastId);
                        line= data[1];
                     } catch (NumberFormatException e) {
                        // ignore, line is kept intact
                     }

                     try {
                        mStringQueue.put (line);
                        mHandler.sendMessage (mHandler.obtainMessage (UPDATE));
                     } catch (InterruptedException e) {
                        Hop.Log("processLog(): put interrputed, there might be missing lines:" + e);
                     }
                  }
               }

               private Integer lastId= 0;
            });
            mPollingThread.setName("Input reader");
            Hop.Log("mPollingThread.start()");
            mPollingThread.start();
        }

        if (mScroll==null) {
            mScroll= (ScrollView) getParent ();
            // Hop.Log("HopLogView: "+mScroll);
        }
        if (mScroll!=null) {
            // Hop.Log("scroll! "+h);
            mScroll.scrollTo (0, h);
        }
    }

    /**
     * Look for new input from the ptty, send it to the terminal emulator.
     */
    private void update() {
        try {
            String line= mStringQueue.take ();
            // Hop.Log("append! "+line);
            append(line+"\n");
        } catch (InterruptedException e) {
        }
    }

}
