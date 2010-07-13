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
        mStringQueue = new ArrayBlockingQueue<String>(10);
        rd= null;

        try {
            // V/hop-installer(20564): HopLogView():java.net.ConnectException: localhost/127.0.0.1:8080 - Connection refused
            logcat= (HttpURLConnection) new URL ("http://localhost:8080/logcat").openConnection ();
            logcat.connect ();
            rd= new BufferedReader (new InputStreamReader (logcat.getInputStream()));
        } catch (MalformedURLException e) {
            // TODO: FAIL
            Hop.Log("HopLogView():" + e);
        } catch (IOException e) {
            Hop.Log("HopLogView():" + e);
            // TODO: FAIL
        }
    }

    protected void onSizeChanged(int w, int h, int oldw, int oldh) {
        super.onSizeChanged (w, h, oldw, oldh);

        Hop.Log("onSizeChanged()" + w + ":" + h);
        if (!mKnownSize && rd!=null) {
            mKnownSize = true;

            // Set up a thread to read input from the socket
            mPollingThread = new Thread(new Runnable() {

                public void run() {
                    try {
                        String line= rd.readLine ();
                        while (line!=null) {
                            mStringQueue.put (line);
                            mHandler.sendMessage (mHandler.obtainMessage (UPDATE));
                            line= rd.readLine ();
                        }
                    } catch (IOException e) {
                    } catch (InterruptedException e) {
                    }
                }
                private byte[] mBuffer = new byte[4096];
            });
            mPollingThread.setName("Input reader");
            mPollingThread.start();
        }
    }

    /**
     * Look for new input from the ptty, send it to the terminal emulator.
     */
    private void update() {
        try {
            String line= mStringQueue.take ();
            append(line);
        } catch (InterruptedException e) {
        }
    }

}
