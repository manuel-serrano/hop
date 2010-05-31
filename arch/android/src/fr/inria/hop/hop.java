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

import android.app.Activity;
import android.os.Bundle;
// import android.os.FileUtils;
// import com.android.dx.util.FileUtils;
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

   /** Called when the activity is first created. */
   @Override
   public void onCreate(Bundle savedInstanceState) {
      super.onCreate(savedInstanceState);
      setContentView(R.layout.unpack);

      mStatus = (TextView) findViewById(R.id.statusText);
      mStatus.setVerticalScrollBarEnabled(true);

      findViewById(R.id.layout).setVerticalScrollBarEnabled(true);

      this.unpack();

      // once we finished, show the terminal running hop
      setContentView(R.layout.terminal);
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
}
