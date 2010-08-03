package fr.inria.hop;

import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Enumeration;
import java.util.Vector;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;
import java.math.BigInteger;
import java.util.Date;
import java.lang.String;
import java.util.ArrayList;
import java.net.URL;
import java.net.HttpURLConnection;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.net.ProtocolException;
import java.util.concurrent.ArrayBlockingQueue;
import java.net.MalformedURLException;
import java.io.FileDescriptor;

import android.app.Activity;
import android.os.Bundle;
import android.util.Log;
import android.app.AlertDialog;
import android.content.DialogInterface;
import android.widget.TextView;
import android.os.Handler;
import android.os.Message;
import android.content.Context;

// some code based on MonoActivity
// http://github.com/koush/androidmono/blob/master/MonoActivity/src/com/koushikdutta/mono/MonoActivity.java

public class Hop extends Object {
   static Runtime mRuntime = Runtime.getRuntime();

   // TODO: use the config
   final static String mAppRoot = "/data/data/fr.inria.hop";
   final static int BUFSIZE = 10*1024; // 10k
   final static String APK_PATH = "/data/app/fr.inria.hop.apk";
   final static String assetsFilter = "assets";
   final static String librarySuffix = ".so";
   final static String libraryInitSuffix = ".init";
   final static String dot_afile = "dot.afile";
   final static File wizard_hop= new File (mAppRoot+"/home/.config/hop/wizard.hop");
   // TODO: let the user select tmpDir
   // Moto Droid: we can write in /tmp and check with any file browser
   final static String tmpDir= "/tmp";
   // cupcake, donut: as we can shell is as root, it doesn't really matter what path we set
   // final static String tmpDir= mAppRoot+"/tmp";

   private final static String DEFAULT_INITIAL_COMMANDS[] = {
      "export HOME=/data/data/fr.inria.hop/home",
      // if we don't redirect the output, hop blocks because nobody reads it
      // so we need tmpDir to exist
      "mkdir "+tmpDir,
      // at least we have exec
      "exec /data/data/fr.inria.hop/bin/hop -v5 -g3 -w3 --verbose-output buffer > "+tmpDir+"/hop.run 2>&1",
   };

   Process p;

    /**
     * The pseudo-teletype (pty) file descriptor that we use to communicate with
     * another process, typically a shell.
     */
    private FileDescriptor mShellFd;

   /**
    * Used to send data to the remote process. Needed to implement the various
    * "report" escape sequences.
    */
   private FileOutputStream mCommandFd;

   public static void Log(String string) {
      Log.v("Hop", string);
   }

   public void copyStreams(InputStream is, FileOutputStream fos) {
      BufferedOutputStream os = null;
      try {
         byte data[] = new byte[BUFSIZE];
         int count;
         os = new BufferedOutputStream(fos, BUFSIZE);
         while ((count = is.read(data, 0, BUFSIZE)) != -1) {
               os.write(data, 0, count);
         }
         os.flush();
      } catch (IOException e) {
         Log("Exception while copying: " + e);
      } finally {
         try {
               if (os != null) {
                  os.close();
               }
         } catch (IOException e2) {
               Log("Exception while closing the stream: " + e2);
         }
      }
   }

   public void unpack() {
      File zipFile = new File(APK_PATH);
      long zipLastModified = zipFile.lastModified();
      try {
         ZipFile zip = new ZipFile(APK_PATH);

         Vector<ZipEntry> files = filesFromZip(zip);
         int zipFilterLength = assetsFilter.length();

         Enumeration entries = files.elements();
         while (entries.hasMoreElements()) {
            ZipEntry entry = (ZipEntry) entries.nextElement();
            String path = entry.getName().substring(zipFilterLength);
            // convert dot.afile into .afile
            if (path.endsWith (dot_afile)) {
               path = path.replace (dot_afile, ".afile");
               Log(".afile: "+path);
            }
            if (path.endsWith ("hoprc.hop")) {
               path = path.replace ("config", ".config");
               Log(".config: "+path);
            }
            File outputFile = new File(mAppRoot, path);
            File parent = outputFile.getParentFile();
            parent.mkdirs();
            if (outputFile.exists() && entry.getSize() == outputFile.length() &&
                  zipLastModified < outputFile.lastModified()) {
               Log(outputFile.getName() + " already extracted and not older.");
            } else {
               try {
                  FileOutputStream fos = new FileOutputStream(outputFile);
                  Log(path+" installed...");
                  copyStreams(zip.getInputStream(entry), fos);
                  String curPath = outputFile.getAbsolutePath();
                  do {
                     // TODO: this is heavy on the CPU
                     mRuntime.exec("/system/bin/chmod 755 " + curPath);
                     curPath = new File(curPath).getParent();
                  } while (!curPath.equals(mAppRoot));
               } catch (IOException e) {
                  Log("Error: " + e.toString());
               }
            }
         }
      } catch (IOException e) {
         Log("Error: " + e.toString());
      }

   }

   public Vector<ZipEntry> filesFromZip(ZipFile zip) {
      // we don't copy the whole contents of the .apk file
      // just what is filtered with this function
      Vector<ZipEntry> list= new Vector<ZipEntry>();
      Enumeration entries= zip.entries();
      while (entries.hasMoreElements()) {
         ZipEntry entry= (ZipEntry) entries.nextElement();
         String name= entry.getName();
         if (name.startsWith (assetsFilter)) {
               list.add(entry);
         } else {
               // Log (name + " skipped: not an asset?");
         }
      }
      return list;
   }

   /*
      from weblets/wizard/wizard.hop:
      ~(define (crypt na pd)
         (string-append "+" (digest-password-encrypt na pd (hop-realm))))

      from runtime/password.scm:
      (define (digest-password-encrypt n p r)
         (md5sum (string-append n ":" r ":" p)))

      from runtime/param.scm:
      (define-parameter hop-realm
         "hop")
   */
   public String crypt (String na, String pd) {
      byte[] md5sum= {};
      String ans;

      try {
         java.security.MessageDigest digest= java.security.MessageDigest.getInstance ("MD5");
         md5sum= digest.digest ((na+":hop:"+pd).getBytes());
         ans= "+"+getHex (md5sum);
      } catch (java.security.NoSuchAlgorithmException e) {
         // we really can't do much here
         ans= null;
      }

      return ans;
   }

   public void write (FileOutputStream fos, String line) throws IOException {
      fos.write (line.getBytes ());
      // Log(line);
   }

   public String createAdminUser () {
      // we create the wizard.hop file with an initial admin user
      // whose password is randomly generated
      String password= null;
      String scrambled= null;

      if (firstTime ()) {
         Log("Creating admin user");
         // 32bits==4bytes==8 hexa digits, 4Gi combinations
         password= new BigInteger (32, new java.util.Random ()).toString (16);
         scrambled= crypt ("admin", password);
         Log("pass:  " + password);
         Log("scram: " + scrambled);

         if (scrambled!=null) {
            try {
               FileOutputStream fos= new FileOutputStream (wizard_hop);
               Date now= new Date ();
               String date= now.toString ();

               write (fos, ";; generated file, Hop installer "+date+"\n");
               write (fos, ";; anonymous user\n");
               write (fos, "(add-user! \"anonymous\")\n");

               write (fos, ";; admin\n");
               write (fos, "(add-user! \"admin\" :groups (quote (admin exec)) :password \""+scrambled+"\" :directories (quote *) :services (quote *))\n");
            } catch (IOException e) {
               Log ("can't write "+wizard_hop.getPath ()+": "+e);
               // if we can't set the password
               password= null;
            }
         } else {
            Log("couldn't generate the scrambled password; maybe there's no md5 algrithm?");
            password= null;
         }
      }

      // return the admin password
      return password;
   }

   // copied from http://www.rgagnon.com/javadetails/java-0596.html
   // CC by-nc-sa Réal Gagnon <real@rgagnon.com>
   static final String HEXES = "0123456789abcdef";
   public String getHex( byte [] raw ) {
      if ( raw == null ) {
         return null;
      }
      final StringBuilder hex = new StringBuilder( 2 * raw.length );
      for ( final byte b : raw ) {
         hex.append(HEXES.charAt((b & 0xF0) >> 4)).append(HEXES.charAt((b & 0x0F)));
      }
      return hex.toString();
   }

   final boolean firstTime () {
      return ! wizard_hop.exists ();
   }

   // entry point
   final void init (Activity parent) {
      // TODO: what on updates?
      // BUG: somehow it still thinks any new package is *completely* new
      // can be a bug in 'prepare' or the dates in the zip/apk created
      unpack ();
      if (firstTime ()) {
         String password= createAdminUser ();

         AlertDialog.Builder builder = new AlertDialog.Builder (parent);
         builder.setMessage("Hop has been installed. The admin password is: "+password)
               .setCancelable(false)
               .setPositiveButton("Ok", new DialogInterface.OnClickListener() {
                  public void onClick(DialogInterface dialog, int id) {
                     dialog.dismiss();
                  }
               });
         AlertDialog alert = builder.create ();
         alert.show ();
      } else {
         Log("not the first time!");
      }
   }

   private void sendInitialCommands () {
      for (int i=0; i<DEFAULT_INITIAL_COMMANDS.length; i++) {
         String command = DEFAULT_INITIAL_COMMANDS[i];
         if (command.length() > 0) {
            Log("PreComm: "+command);
            write(command + '\r');
         }
      }
   }

   private void write(String data) {
      try {
         mCommandFd.write(data.getBytes());
         mCommandFd.flush();
      } catch (IOException e) {
         // Ignore exception
         // We don't really care if the receiver isn't listening.
         // We just make a best effort to answer the query.
      }
   }

   final void run () {
        // TODO: try to detect wether hop is already running or not before launching it.
        // Some code from Android Term app
        // http://android.git.kernel.org/?p=platform/development.git;a=blob;f=apps/Term/src/com/android/term/Term.java;hb=HEAD
        // Copyright (C) 2007 The Android Open Source Project
        // Licensed under the Apache License, Version 2.0 (the "License")
        int[] processId = new int[1];

        createSubprocess(processId);
        final int procId = processId[0];

        final Handler handler = new Handler() {
            @Override
            public void handleMessage(Message msg) {
            }
        };

        Runnable watchForDeath = new Runnable() {

            public void run() {
                Hop.Log("waiting for: " + procId);
                int result = HopExec.waitFor(procId);
                Hop.Log("Subprocess exited: " + result);
                handler.sendEmptyMessage(result);
            }

        };
        Thread watcher = new Thread(watchForDeath);
        watcher.start();

        sendInitialCommands ();
   }

    private ArrayList<String> parse(String cmd) {
        final int PLAIN = 0;
        final int WHITESPACE = 1;
        final int INQUOTE = 2;
        int state = WHITESPACE;
        ArrayList<String> result =  new ArrayList<String>();
        int cmdLen = cmd.length();
        StringBuilder builder = new StringBuilder();
        for (int i = 0; i < cmdLen; i++) {
            char c = cmd.charAt(i);
            if (state == PLAIN) {
                if (Character.isWhitespace(c)) {
                    result.add(builder.toString());
                    builder.delete(0,builder.length());
                    state = WHITESPACE;
                } else if (c == '"') {
                    state = INQUOTE;
                } else {
                    builder.append(c);
                }
            } else if (state == WHITESPACE) {
                if (Character.isWhitespace(c)) {
                    // do nothing
                } else if (c == '"') {
                    state = INQUOTE;
                } else {
                    state = PLAIN;
                    builder.append(c);
                }
            } else if (state == INQUOTE) {
                if (c == '\\') {
                    if (i + 1 < cmdLen) {
                        i += 1;
                        builder.append(cmd.charAt(i));
                    }
                } else if (c == '"') {
                    state = PLAIN;
                } else {
                    builder.append(c);
                }
            }
        }
        if (builder.length() > 0) {
            result.add(builder.toString());
        }
        return result;
    }

    private void createSubprocess(int[] processId) {
        String shell = "/system/bin/sh -";
        ArrayList<String> args = parse(shell);
        String arg0 = args.get(0);
        String arg1 = null;
        String arg2 = null;
        String arg3 = null;
        String arg4 = null;
        String arg5 = null;
        if (args.size() >= 2) {
            arg1 = args.get(1);
        }
        if (args.size() >= 3) {
            arg2 = args.get(2);
        }
        if (args.size() >= 4) {
            arg3 = args.get(3);
        }
        if (args.size() >= 5) {
            arg4 = args.get(4);
        }
        if (args.size() >= 6) {
            arg5 = args.get(5);
        }
        mShellFd= HopExec.createSubprocess (arg0, arg1, arg2, arg3, arg4, arg5, processId);
        mCommandFd= new FileOutputStream (mShellFd);
    }
}
