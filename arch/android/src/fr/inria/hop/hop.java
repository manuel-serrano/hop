package fr.inria.hop;

import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Enumeration;
import java.util.Vector;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;
import java.math.BigInteger;
import java.util.Date;

import android.app.Activity;
import android.os.Bundle;
import android.util.Log;
import android.widget.TextView;

// shamelessly copied from MonoActivity
// http://github.com/koush/androidmono/blob/master/MonoActivity/src/com/koushikdutta/mono/MonoActivity.java

public class hop extends Activity {
   static Runtime mRuntime = Runtime.getRuntime();
   final static String mAppRoot = "/data/data/fr.inria.hop";
   final static int BUFSIZE = 10*1024; // 10k
   final static String APK_PATH = "/data/app/fr.inria.hop.apk";
   final static String assetsFilter = "assets";
   final static String librarySuffix = ".so";
   final static String libraryInitSuffix = ".init";
   final static String dot_afile = "dot.afile";
   TextView mStatus;

   public static void Log(String string) {
      Log.v("hop-installer", string);
      // mStatus.append(string+"\n");
   }

   public static void copyStreams(InputStream is, FileOutputStream fos) {
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

   public static void unpack() {
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
            if (path.endsWith (dot_afile)) {
               path = path.substring(0, path.length()-dot_afile.length())+".afile";
               Log(".afile: "+path);
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

   public static Vector<ZipEntry> filesFromZip(ZipFile zip) {
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
   public static String crypt (String na, String pd) {
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

   public static String createAdminUser () {
      // we create the hoprc.hop file with an initial admin user
      // whose password is randomly generated

      Log("Creating admin user");
      // 32bits==4bytes==8 hexa digits, 4Gi combinations
      String password= new BigInteger (32, new java.util.Random ()).toString (16);
      String scrambled= crypt ("admin", password);
      Log("pass:  " + password);
      Log("scram: " + scrambled);

      if (scrambled!=null) {
         File wizard_hop= new File (mAppRoot+"/home/.config/hop/wizard.hop");

         try {
            FileOutputStream fos= new FileOutputStream (wizard_hop);
            Date now= new Date ();
            String date= now.toString ();

            fos.write ((";; generated file, Hop installer "+date+"\n").getBytes ());
            fos.write (";; anonymous user\n".getBytes ());
            fos.write ("(add-user! \"anonymous\")\n".getBytes ());

            fos.write (";; admin\n".getBytes ());
            fos.write (("(add-user! \"admin\" :groups (quote (admin exec)) :password \""+scrambled+"\" :directories (quote *) :services (quote *))\n").getBytes ());
         } catch (IOException e) {
            // if we can't set the password
            password= null;
         }
      } else {
         password= null;
      }

      // return the admin password
      return password;
   }

   // copied from http://www.rgagnon.com/javadetails/java-0596.html
   // CC by-nc-sa Réal Gagnon <real@rgagnon.com>
   static final String HEXES = "0123456789abcedf";
   public static String getHex( byte [] raw ) {
      if ( raw == null ) {
         return null;
      }
      final StringBuilder hex = new StringBuilder( 2 * raw.length );
      for ( final byte b : raw ) {
         hex.append(HEXES.charAt((b & 0xF0) >> 4)).append(HEXES.charAt((b & 0x0F)));
      }
      return hex.toString();
   }
}
