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

public class hop_2_0_0 extends Activity {
    static Runtime mRuntime = Runtime.getRuntime();
    final static String mAppRoot = "/data/data/fr.inria.hop";
    final static int BUFSIZE = 10000;
    final static String APK_PATH = "/data/app/fr.inria.hop.apk";
    final static String ZIP_FILTER = "assets";
    TextView mStatus;

    public void Log(String string) {
        Log.v("hop-installer", string);
        mStatus.setText(mStatus.getText() + "\n" + string);
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

    /** Called when the activity is first created. */
    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.main);

        mStatus = (TextView) findViewById(R.id.StatusText);
        mStatus.setVerticalScrollBarEnabled(true);

        findViewById(R.id.LinearLayout).setVerticalScrollBarEnabled(true);

        try {
            File zipFile = new File(APK_PATH);
            long zipLastModified = zipFile.lastModified();
            ZipFile zip = new ZipFile(APK_PATH);
            Vector<ZipEntry> files = pluginsFilesFromZip(zip);
            int zipFilterLength = ZIP_FILTER.length();

            Enumeration entries = files.elements();
            while (entries.hasMoreElements()) {
                ZipEntry entry = (ZipEntry) entries.nextElement();
                String path = entry.getName().substring(zipFilterLength);
                File outputFile = new File(mAppRoot, path);
                outputFile.getParentFile().mkdirs();

                if (outputFile.exists() && entry.getSize() == outputFile.length() &&
                  zipLastModified < outputFile.lastModified()) {
                    Log(outputFile.getName() + " already extracted.");
                } else {
                    FileOutputStream fos = new FileOutputStream(outputFile);
                    Log("Copied " + entry + " to " + mAppRoot + "/" + path);
                    copyStreams(zip.getInputStream(entry), fos);
                    String curPath = outputFile.getAbsolutePath();
                    do {
                        mRuntime.exec("/system/bin/chmod 755 " + curPath);
                        curPath = new File(curPath).getParent();
                    } while (!curPath.equals(mAppRoot));
                }
            }
            cachePackageSharedLibsLI (zipFile);
        } catch (IOException e) {
            Log("Error: " + e.getMessage());
        }
    }

    void cachePackageSharedLibsLI(File scanFile) throws IOException {
        // File sharedLibraryDir = new File(dataPath.getPath() + "/lib");
        final String sharedLibraryABI = "armeabi";
        final String apkLibraryDirectory = "lib/" + sharedLibraryABI + "/";
        final String apkSharedLibraryPrefix = apkLibraryDirectory + "lib";
        final String sharedLibrarySuffix = ".so";
        final String libraryInitSuffix = ".init";
        boolean createdSharedLib = false;
        try {
            Log ("scanning package for shared libs");
            ZipFile zipFile = new ZipFile(scanFile);
            Enumeration<ZipEntry> entries =
                (Enumeration<ZipEntry>) zipFile.entries();

            while (entries.hasMoreElements()) {
                ZipEntry entry = entries.nextElement();
                if (entry.isDirectory()) {
                    Log (entry.getName ()+" skipped: DIR");
                    continue;
                }
                String entryName = entry.getName();
                if (! (entryName.startsWith(apkSharedLibraryPrefix)
                        && entryName.endsWith(sharedLibrarySuffix))) {
                    Log (entry.getName ()+" skipped: does not match "+apkSharedLibraryPrefix+"*"+sharedLibrarySuffix);
                    continue;
                }
                String libFileName = entryName.substring(
                        apkLibraryDirectory.length());
                if (libFileName.contains("/")) {
                        // || (!FileUtils.isFilenameSafe(new File(libFileName)))) {
                    Log (entry.getName ()+" skipped: has / or not safe.");
                    continue;
                }
                /*
                String sharedLibraryFilePath = sharedLibraryDir.getPath() +
                    File.separator + libFileName;
                File sharedLibraryFile = new File(sharedLibraryFilePath);
                if (! sharedLibraryFile.exists() ||
                    sharedLibraryFile.length() != entry.getSize() ||
                    sharedLibraryFile.lastModified() != entry.getTime()) {
                    if (Config.LOGD) {
                        Log.d(TAG, "Caching shared lib " + entry.getName());
                    }
                    if (mInstaller == null) {
                        sharedLibraryDir.mkdir();
                        createdSharedLib = true;
                    }
                    cacheSharedLibLI(pkg, zipFile, entry, sharedLibraryDir,
                            sharedLibraryFile);
                }
                */
            }
        } catch (IOException e) {
            Log.e("hop-shlib-", "Failed to cache package shared libs", e);
            /*
            if(createdSharedLib) {
                sharedLibraryDir.delete();
            }
            */
            throw e;
        }
    }

    public Vector<ZipEntry> pluginsFilesFromZip(ZipFile zip) {
        Vector<ZipEntry> list = new Vector<ZipEntry>();
        Enumeration entries = zip.entries();
        while (entries.hasMoreElements()) {
            ZipEntry entry = (ZipEntry) entries.nextElement();
            if (entry.getName().startsWith(ZIP_FILTER)) {
                list.add(entry);
            }
        }
        return list;
    }
}
