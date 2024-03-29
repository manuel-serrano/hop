Android Packaging
=================

_22 Feb 2022_


This document explains how to compile Hop for Android devices and how
to package it in an `apk` file. We _also_ show how to build an Hop
application that runs inside an HTML container on the devices.


Disclaimer
----------

Developping for the Android platform is hard. It requires skills,
practices, and experience. Debugging is generally difficult, error
messages and exception handling is rudimentary, and development cycle
slow. Hop tries it best to simplify the process but it cannot hide
all the complexity of the Android environment. At the very least, 
you will have to understand how to create your own application
[manifest](https://developer.android.com/guide/topics/manifest/manifest-intro)
and how to customize the application 
[layout](https://developer.android.com/guide/topics/resources/layout-resource).

Only a tiny subset of Android features are currently available in Hop.
These are documented in this page. The Hop Scheme layer is generally more
advanced because bindings are first created for Scheme and then ported
to JavaScript.


Requirements
------------

The development method used by Hop requires the devices to be 
in [developper mode](https://developer.android.com/studio/debug/dev-options).

Complete Android `sdk` and `ndk` toolkits are needed. Check the Bigloo
Android [cross compilation instructions](http://www-sop.inria.fr/indes/fp/Bigloo/cross.html) for
installing the needed packages.

To build the Hop `apk` you will only need the following Android tools:

  * `ndk-build`
  * `aapt`
  * `dx`
  * `zipalign`
  * `adb`
  
The Makefiles Hop are sufficient to build the package. That is, `ant`,
`cmake`, or `gradle` or _not_ needed.

The Android port is **experimental** and incomplete. The restrictions
are:

  * On older Android version API < 22, only static libraries are supported.
 Only more recent versions support dynamic linking and dynamic loading.
  * The Hop packaging procedure generates `apk` that can only be execued
 on one target archiecture. That is, a single `apk` cannot contain binary
 code for, let us say, arm5, x86, and arm64.
  * Each application must be packaged with its own private Hop version.
 This waste storage space on the device.
  * Hop application can access to device feature via a plugin mecanism. For
 instance, phone apps can access to the messaging service (sms), phone 
 facilities, multmedia resources, etc. Not all Android API are currently
 supported and additional plugins should be implemented.

In addition, you will need the 
 [Bigloo](http://www-sop.inria.fr/indes/fp/Bigloo/download.html) 
and 
 [Hop](http://hop.inria.fr/home/download.html)
source file distributions (tarball files). In the rest, we assume that these
files are stored in the `your-repodir` directory


Default Build
-------------

To build a full hop+bigloo stack and to produce an `apk` file:

    $ cd arch/android
    $ export REPODIR=your-repodir
    $ make apk 
	
The directory `repodir` must contain the tarball of the Bigloo version used
to build and apk.

To build for debugging:

    $ make apk HOPCONFIGUREOPT=--debug

This will use the android sdk and ndk pointed to by the environment variables
`$ANDROIDSDK` and `$ANDROIDNDK`. The default configuration can be 
changed with `make` arguments. For intsance:

    $ make apk ANDROIDNDK=/opt/android/android-ndk-r21b

The default configuration assumes a physiscal device connected to
the USB bus. If an emulator is used, add the following to all
`make` invocation:

    $ make apk ANDROIDHOST=-e


If only hop needs to be reinstalled and recompiled:

    $ make apk-sans-bigloo

If only hop needs to be recompiled (after a modification in the Hop build dir):

    $ make apk-re-hop

If only the `apk` file has be to regenerated:

    $ make apk-sans-hop

When preparing an `apk` for an old Android version (api-level < 22), 
static linking must be forced. 

    $ make ANDROIDLINK=static

For additional configuration, check `arch/android/Makefile`, in particular
the variables `ANDROIDTARGET` and `ANDROIDSDKVERSION` that control
the targetted Android version.


Installing
----------

The standard `adb` Android tool tool is used to installed:

    $ adb install -r hop.apk
    $ adb shell monkey -p fr.inria.hop 1

To uninstall it:

    $ adb uninstall fr.inria.hop

To stop it:

    $ adb shell am force-stop fr.inria.hop

To run Hop in debug mode

    $ adb shell am start -n fr.inria.hop/.HopLauncher -ei -g 4 -ei -v 2
	
To stop and restart:

    $ adb shell am force-stop fr.inria.hop && adb shell am start -n fr.inria.hop/.HopLauncher -ei --g 4 --ei -v 2

To switch the screen on:

    $ adb shell input keyevent KEYCODE_POWER
	

Building a Client Application
-----------------------------


Building an Bundled Custom Application
---------------------------------------

In some situations, it is desirable to build a Hop application bundled
with a specific Hop instance. The section explains how to construct
such an `apk` containing a bare `hop` installation plus a client 
application. The makefile located in the `arch/android` is
designed so that it can be used to build custom application. To add a
weblet to your `apk` build, you just have to specify an
`ANDROIDWEBLET` argument to the `make` command line that must point to
the directory containing your weblet. Let us show how to build such a
complete application. The 3 steps will be:

  1. create the `arch/android` directory;
  2. create the `Makefile` needed to build the `apk`;
  3. invoke `make` to build the package.
  
We detail these steps in the following. First, let's ensure that we
have properly defined the two environment variables needed to 
find the location on our disk of the Android sdk and ndk:

    $ export ANDROIDSDK=.../android-sdk-linux
    $ export ANDROIDNDK=.../android-ndk-r21b

Second create the directory
that will contain our weblet source files. Normally you already have this.

    $ mkdir hopdemo; cd hopdemo
	
We create a simple service for the demonstration `hopdemo.js`:

```hopscript
const fs = require( "fs" );
const path = require( "path" );

service hopdemo( arg ) {
   const dir = (arg && arg.dir) || "/mnt/sdcard/home";
   return <html>
     <head>
       <style>body { font-size: 200% }</style>
     </head>
     <body>
       <div> ${dir} </div>
       <ul> 
       	 <li><a href=${hopdemo( { dir: path.dirname( dir ) } )}>..</a></li>
       	 ${ fs.readdirSync( dir )
	       .map( p => {
		  const fp = path.join( dir, p );
		  if( fs.lstatSync( fp ).isDirectory() ) {
		     return <li><a href=${hopdemo( { dir: fp } )}> ${p} </a></li>;
		  } else {
		     return <li><a href=${fp}>${p} (${fs.lstatSync( fp ).size})</a></li>
		  }
	       } ) }
       </ul>
     </body>
   </html>
}
```

Of course, it's also possible to define the same weblet in Scheme. The
source file should be `hopdemo.hop`:

```scheme
(module hopdemo)

(define-service (hopdemo #!optional (dir "/mnt/sdcard/home"))
   (<HTML>
      (<HEAD> (<STYLE> [body { font-size: 200% }]))
      (<DIV> dir)
      (<UL>
         (<LI> (<A> :href (hopdemo (dirname dir)) ".."))
         (map (lambda (p)
                 (<LI>
                    (if (directory? p)
                        (<A> :href (hopdemo p) (basename p))
                        (<A> :href p (basename p) " " (file-size p)))))
            (directory->path-list dir)))))
```

In the rest of this document we focus on the JavaScript backend.

We now create the directory for the Android package:

    $ mkdir -p arch/android; cd arch/android
	
Then we create the Makefile needed to build the `apk`. For convenience,
we split it in two files. First a Makefile containing only the application
configuration:

    $ cat > Makefile.config <<EOF
    HOPDIR = $$HOME/prgm/project/hop/hop
    ANDROIDWEBLET = $(shell realpath $$PWD/../..)
    
    HOPAPKVERSION = 1.0.0
    HOPAPP = hopdemo
    HOPAPKNAME = $(HOPAPP)-$(HOPAPKVERSION)

    # the list of hopdroid plugins the application will need
    PLUGINBUILD = true
    PLUGINLOCALE = false
    PLUGINVIBRATE = false
    PLUGINMUSICPLAYER = true
    PLUGINMEDIAAUDIO = true
    PLUGINSENSOR = false
    PLUGINBATTERY = false
    PLUGINSMS = false
    PLUGINWIFI = false
    PLUGINCONNECTIVITY = false
    PLUGINCONTACT = false
    PLUGINZEROCONF = true
    PLUGINSYSTEM = false
    PLUGINTTS = false
    PLUGINCALL = false
    PLUGINPREFS = true

Second the Makefile to build the application.

    $ cat > Makefile.android <<EOF
    include Makefile.config
    
    apk: 
        $(MAKE) -C $(HOPDIR)/arch/android apk \
            ANDROIDWEBLET=$(ANDROIDWEBLET) \
            CONFIG=$(ANDROIDWEBLET)/arch/android/Makefile.config
    
    apk-sans-bigloo: doit
    apk-sans-hop: doit
    install-apk: doit
    
    doit:
        $(MAKE) -C $(HOPDIR)/arch/android $(MAKECMDGOALS) \
            ANDROIDWEBLET=$(ANDROIDWEBLET) \
            CONFIG=$(ANDROIDWEBLET)/arch/android/Makefile.config
    
    prepare-android-weblet:
    	find . -name '*.o' -exec /bin/rm {} \;
    	find . -name '*.so' -exec /bin/rm {} \;
    	rm -rf ../../arch/android
    EOF

Note that the name `Makefile.android` is mandatory. It cannot be changed.
We can now build our `apk` with the following:

    $ make -f Makefile.android

This generate a file name `/tmp/build.hopdemo-1.0.0.android/hopdemo-1.0.0.apk`
that can be installed on the device as explained earlier.

Customizing a Custom Application
--------------------------------

The procedure shown before builds standard packages with default Hop
Android configurations. This default configuration can be customized
in two ways. An altnerative [Manifest](https://developer.android.com/guide/topics/manifest/manifest-intro)
can be given. For that, create a `AndroidManifest.xml.in` (pay
attention to the `.xml.in` suffix) file and extend your `Makefile.config` 
file as follows:

    $ echo "ANDROIDMANIFEST=PROJDIR/AndroidManifest.xml.in` >> Makefile.config

Check the Hop orginal `arch/android/AndroidManifest.xml.in` to create
your. This file follows the Android syntax and semantics but in addition
the _@-keywords_, e.g., `@HOPVERSION@`, will be replaced
by their actual values during the package construction.

** Note **: As `make` rely n a sort of lazy evaluation, if you define 
`ANDROIDMANIFEST` as `$$PWD/AndroidManifest.xml.in`, the environment
variable will `PWD` be evaluated on the hop directory and not the directory
containing your project, which will yield to ignoring your configuration. There
are two options for solving that problem:

  1. use an absolute file name for `ANDROIDMANIFEST`;
  2. force an eager evaluation of the variable by passing it to 
 the sub `make` command with:
  
    apk: 
        $(MAKE) -C $(HOPDIR)/arch/android apk \
            ANDROIDWEBLET=$(ANDROIDWEBLET) \
            ANDROIDMANIFEST=$(ANDROIDMANIFEST) \
            CONFIG=$(ANDROIDWEBLET)/arch/android/Makefile.config


The icons and other graphical elements of an Android application are
stored in the `res` directory. Hop uses a set of default icons that
can be changed by providing a replacement directory. For that, extend
the `Makefile.config` file as follows:

    $ echo "ANDROIDRES=PROJDIR/res` >> Makefile.config


Some projects rely on particular libraries that needs to be installed
after Hop but before the Androd application. This is the purpose of
`APKCUSTOM` make variable. It can be assigned to an application goal
that will be executed by the Android make invocation. Example:

    $ echo "APKCUSTOM=custom-proj-compile-and-install` >> Makefile.config


Using the hopdroid package
--------------------------

The `hopdroid` [package](./hopdroid.html) enables JavaScript programs
to access device specific features. Example:

```hopscript
const hopdroid = require( hop.hopdroid );
const phone = new hopdroid.phone();

service hopdemo() {
   console.log( "phone=", phone.model, phone.product, phone.sdk );
   return <html>
     <head>
       <style>body { font-size: 200% }</style>
     </head>
     <body>
       <div> ${phone.model}, ${phone.product}, ${phone.sdk} </div>
     </body>
   </html>;
}

```

Simulating
----------

Hop Android applications can be executed out-of-device. The whole Android
API exists and can be executed but it delivers fake values and fake
behaviour. This simulation mode can be of great help for developping an
application because it lets programmers develop the application on
their regular development platform and install it on the phone only
when the application is mostly operational.

To use the simulator, spawn your application as usual on you development
machine. Then, within a browser running on the same machine, access
the file `/hop/hopdroid/simulator`. This page will let you control the
state of the simulated phone. 

Note: the simulator only supports _one_ phone object at a time. That
is, the simulator will only let you access the state of the last created
simulated phone.


Debugging
---------

Here are some hints for debugging the application running on the device.
The first is the logging facility offered by `adb`. On new devices:

    $ adb logcat | grep '[VDIE] [Hh]op'
	
or 

	$ adb logcat -e 'Hop'

	
This will show all Java message and also all the output of the application,
in particular, all the application write on its standard output and standard
error ports.

Sometime it's useful to execute commands on the device. The Android protection
mechanism makes it's difficult to access the files belonging to the 
application. For that use the `adb exec-out run-as fr.inria.hop` command.
Examples:

    $ adb exec-out run-as fr.inria.hop ls /data/data/fr.inria.hop/assets/bin/hop
    $ adb exec-out run-as fr.inria.hop /system/bin/sh -c "export HOME=/sdcard/home; export LD_LIBRARY_PATH=/data/data/fr.inria.hop/assets/lib/bigloo/4.5a:/data/data/fr.inria.hop/assets/lib/hop/3.6.0:$LD_LIBRARY_PATH;exec /data/data/fr.inria.hop/assets/bin/hop --no-color -p 8080 -g0 --max-threads 6 -z --no-jobs --rc-dir /sdcard/home/.config/hop -v2"

If more debugging is needed, `gdb` can be used remotely. For that, proceed
as follows:

    $ pid=PID
    $ port=5050
    $ adb shell run-as fr.inria.hop /data/data/fr.inria.hop/lib/gdbserver ":$port" --attach $pid

    $ adb forward tcp:$port tcp:$port
    $ gdb app_process
    (gdb) target remote :$port


Androidx
--------

Androidx is a library of extra features that are not pre-installed on
the phone. The `androidx` classes then have to be shipped with applications.
The `jar` files can be found on the official Google Maven

  [Repository](https://maven.google.com/web/index.html)
  
For instance, `androidx` core jar file is 

  [core-1.3.2](https://dl.google.com/android/maven2/androidx/core/core/1.3.2/core-1.3.2-sources.jar)
  
To use this set of classes in an application, download the jar file 
and add the following declaration to the `Makefile.config` of your
application

  ANDROIDX=<the-location-of-the-androidx-jar-file>
  
For instance

  ANDROIDX=/home/myproject/hopapp/androidx/core-1.3.2-sources.jar
