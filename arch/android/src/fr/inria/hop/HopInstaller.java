/*=====================================================================*/
/*    .../hop/hop/arch/android/src/fr/inria/hop/HopInstaller.java      */
/*    -------------------------------------------------------------    */
/*    Author      :  Marcos Dione & Manuel Serrano                     */
/*    Creation    :  Fri Oct  1 08:46:18 2010                          */
/*    Last change :  Thu Dec 31 08:12:55 2020 (serrano)                */
/*    Copyright   :  2010-20 Marcos Dione & Manuel Serrano             */
/*    -------------------------------------------------------------    */
/*    Install Hop (from the zip file).                                 */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    The package                                                      */
/*---------------------------------------------------------------------*/
package fr.inria.hop;

import java.util.*;
import java.util.zip.*;
import java.io.*;

import android.app.Activity;
import android.util.Log;
import android.app.ProgressDialog;
import android.content.DialogInterface;
import android.content.Context;
import android.os.*;

/*---------------------------------------------------------------------*/
/*    HopInstall ...                                                   */
/*---------------------------------------------------------------------*/
public class HopInstaller implements HopStage {
   // global constants
   final static String DOTAFILE = "dot.afile";
   final static String JSGZ = "jsgz";
   final static String ASSETS = "assets";
   final static int BUFSIZE = 10 * 1024;
   final static String CHMOD = "/system/bin/chmod 755";
   
   // instance variables
   Handler handler;
   Boolean isClientInstaller = false;
   String apk;
   String root;
   ProgressDialog progress;
   Activity activity;

   String chmodbuf = "";
   int chmodbuflen = 0;

   Boolean abort = false;

   // constructor
   public HopInstaller( Activity a, Handler h, String hopapk, String hopdir ) {
      super();

      handler = h;
      apk = hopapk;
      root = hopdir;
      activity = a;

      progress = makeProgressBar( a );
   }
   
   public HopInstaller( Activity a, Handler h, String hopapk, String hopdir, Boolean isclient ) {
      super();

      handler = h;
      apk = hopapk;
      root = hopdir;
      activity = a;
      isClientInstaller = isclient;

      progress = makeProgressBar( activity );
   }

   // makeProgressBar
   private ProgressDialog makeProgressBar( Activity activity ) {
      ProgressDialog p = new ProgressDialog( activity );
      p.setTitle( "Hop Installer" );
      p.setMessage( "Unpacking..." );
      p.setMax( 100 );
      p.setProgressStyle( ProgressDialog.STYLE_HORIZONTAL );
      return p;
   }
   
   // static method
   public static boolean installed( String root ) {
      return new File( root, "etc/androidhome.hop" ).exists();
   }
   
   // chmodflush
   private void chmodflush() throws IOException {
      if( chmodbuflen > 0 ) {
	 Runtime.getRuntime().exec( CHMOD + " " + chmodbuf );
	 chmodbuf = "";
	 chmodbuflen = 0;
      }
   }

   // dummy chmod
   private void chmod( String path ) throws IOException {
      chmodbuf += path + " ";

      if( chmodbuflen++ > 10 ) chmodflush();
   }
   
   // some filename have been mangled in the zip file
   private static String patchHopFilename( String path ) {
      if( path.endsWith( DOTAFILE ) ) {
	 return path.replace( DOTAFILE, ".afile" );
      } else if( path.endsWith( JSGZ ) ) {
	 return path.replace( JSGZ, "js.gz" );
      } else if( path.endsWith( "hoprc.hop" ) ) {
	 return path.replace( "config", ".config" );
      } else if( path.endsWith( "hoprc.js" ) ) {
	 return path.replace( "config", ".config" );
      } else {
	 return path;
      }
   }

   // create a directory with the correct mod
   private void mkdir( File dir ) throws IOException {
      String pdir = dir.getAbsolutePath();

      dir.mkdirs();

      do {
	 chmod( pdir );
	 pdir = new File( pdir ).getParent();
      } while( !pdir.equals( root ) );
   }

   // delete a directory recursively
   private void rmdir( File dir ) throws IOException {
      String[] entries = dir.list();

      if( entries != null ) {
	 for( String s: entries ) {
	    File currentFile = new File( dir.getPath(), s );
	    if( currentFile.isDirectory() ) {
	       rmdir( currentFile );
	    } else {
	       currentFile.delete();
	    }
	 }
      }
      dir.delete();
   }
   
   // extract one stream from the zip archive
   public void copyStreams( InputStream is, FileOutputStream fos )
      throws IOException {
      BufferedOutputStream os = null;
      
      try {
         byte data[] = new byte[ BUFSIZE ];
         int count;
         os = new BufferedOutputStream( fos, BUFSIZE );
         while( (count = is.read( data, 0, BUFSIZE) ) != -1 ) {
	    os.write(data, 0, count);
         }
         os.flush();
      } finally {
	 if( os != null ) os.close();
      }
   }
   
   // filter out from the files list all those that are not under
   // the assets directory
   public Vector<ZipEntry> filesFromZip( ZipFile zip ) {
      Vector<ZipEntry> list = new Vector<ZipEntry>();
      Enumeration entries = zip.entries();
      while( entries.hasMoreElements() ) {
         ZipEntry entry = (ZipEntry)entries.nextElement();
         String name = entry.getName();
	 
         if( name.startsWith( ASSETS ) ) list.add(entry);
      }
      
      return list;
   }

   // unpacking the zip file
   public void unpack() throws IOException {
      File zipFile = new File( apk );

      Log.d( "HopInstaller", "###############################################" );
      Log.d( "HopInstaller", HopConfig.APP + " ("
	     + java.time.LocalDate.now() + " "
	     + java.time.LocalTime.now() + ")" );
      Log.d( "HopInstaller", "###############################################" );
      
      Log.d( "HopInstaller", "unpacking: apk=" + apk + " root=" + root );
      
      if( !zipFile.exists() ) {
	 Log.e( "HopInstaller", "file not found: " + apk );
	 throw new FileNotFoundException( apk );
      }
      
      long zipLastModified = zipFile.lastModified();
      ZipFile zip = new ZipFile( apk );
      Vector<ZipEntry> files = filesFromZip( zip );
      final Hashtable dirtable = new Hashtable();
      Enumeration entries = files.elements();
      int i = 0;

      Log.d( "HopInstaller", "progress.setMax " + files.size() );
      progress.setMax( files.size() );
      
      while( entries.hasMoreElements() ) {
	 synchronized( abort ) {
	    if( !abort ) {
	       ZipEntry entry = (ZipEntry) entries.nextElement();
	       String path = entry.getName().substring( ASSETS.length() );

	       progress.setProgress( i++ );
	       if( i % 100 == 0 ) {
		  Log.d( "HopInstaller", "unpacking " + i + "..." );
	       }

	       // restore Hop file names
	       path = patchHopFilename( path );

	       // copy the new file
	       File outputFile = new File( root, path );
	       File dir = outputFile.getParentFile();
	    
	       if( !dirtable.containsKey( dir ) ) {
		  dirtable.put( dir, new Boolean( true ) );
	    
		  if( !dir.isDirectory() ) {
		     mkdir( dir );
		  }
	       }

	       if( outputFile.exists()
		   && (entry.getSize() == outputFile.length())
		   && (zipLastModified < outputFile.lastModified()) ) {
		  ;
	       } else {
		  FileOutputStream fos = new FileOutputStream( outputFile );

		  copyStreams( zip.getInputStream( entry ), fos );
		  String curPath = outputFile.getAbsolutePath();
		  chmod( curPath );
	       }
	    }
	 }
      }

      // we have to execute the chmod mode now
      synchronized( abort ) {
	 if( !abort ) {
	    chmodflush();
	 }
      }
   }

   // create the androidhome.hop file which is read by Hop at start time
   // (see configure-android.sch.in). It contains the path of the
   // external storage so that hop can avoid using the explicit "/sdcard" path.
   void androidhome() throws IOException {
      synchronized( abort ) {
	 if( !abort ) {
	    File file = new File( root, "etc/androidhome.hop" );
	    OutputStream op = new FileOutputStream( file );
	    Log.i( "HopInstaller", "generating androidhome \"" + file + "\"" );
      
	    op.write( ";; generated file (HopInstaller), don't edit\n".getBytes() );
	    op.write( "\"".getBytes() );
	    // MS CARE
	    op.write( HopConfig.HOME.getBytes() );
	    op.write( "\"\n".getBytes() );
	    op.flush();
	    op.close();
	 }
      }
   }

   private void raise( Exception e ) {
      String msg = e.getMessage();
      
      Log.e( "HopInstaller", e.toString() );
      e.printStackTrace();
      
      if( msg == null ) msg = e.getClass().getName();
      
      handler.sendMessage( android.os.Message.obtain( handler, HopLauncher.MSG_INSTALL_FAIL, e ) );
   }
      
   // exec
   public void exec( Context context, Object arg ) {
      Log.i( "HopInstaller",
	     "isClientInstaller=" + isClientInstaller.toString()
	     + " root=" + root );

      if( !installed( root ) ) {
	 try {
	    Thread installer = new Thread( new Runnable () {
		  public void run() {
		     try {
			unpack();
			if( !isClientInstaller ) {
			   Log.d( "HopInstaller", "setting exec mode: " + root + "/bin/hop" );
			   androidhome();
			   chmod( root + "/bin/hop" );
			}
			chmodflush();
			
			handler.sendEmptyMessage( HopLauncher.MSG_INSTALL_UNPACKED );
		     } catch( Exception e ) {
			raise( e );
		     } finally {
			progress.dismiss();
		     }
		  }
	       } );

	    progress.setMessage( "Unpacking..." );
	    progress.show();
	    installer.start();
	 } catch( Exception e ) {
	    raise( e );
	 }
      } else {
	 Log.d( "HopInstaller", "hop already installed=" + installed( root ) );
	 handler.sendEmptyMessage( HopLauncher.MSG_INSTALL_UNPACKED );
      }
   }

   public void abort() {
      Log.d( "HopInstaller", "abort" );
      try {
	 synchronized( abort ) {
	    abort = false;
	    rmdir( new File( root ) );
	 }
      } catch( Exception e ) {
	 raise( e );
      }
   }
}
