Android Packaging
-----------------

_15 May 2020_


This document explains how to compile Hop for Android devices and how
to package it in an `apk` file.

In this document we explain how to build a package that contains a
full Hop installation _and_ an Hop application that we will executed
within an Android HTML application.


Requirements
============

Complete Android `sdk` and `ndk` toolkit are needed. Check the Bigloo
Android [cross compilation
instructions](http://www-sop.inria.fr/indes/fp/Bigloo/cross.html) for
installing the needed packages.

To build the Hop `apk` you will only need the following Android tools:

  * `ndk-build`
  * `aapt`
  * `dx`
  * `zipalign`
  
The Makefiles Hop are sufficient to build the package. That is, `ant`,
`cmake`, or `gradle` or _not needed_.

The Android port is **experimental** and incomplete. The restrictions
are:

  * On older Android version API <= 21, only static libraries are supported.
 Only more recent versions support dynamic linking and dynamic loading.
  * The Hop packaging procedure generates `apk` that can only be execued
 on one target archiecture. That is, a single `apk` cannot contain binary
 code for, let us say, arm5, x86, and arm64.
  * Each Hop application must be packaged with its own private Hop version.
 This waste storage space on the device.
  * Hop application can access to device feature via a plugin mecanism. For
 instance, phone apps can access to the messaging service (sms), phone 
 facilities, multmedia resources, etc. Not all Android API are currently
 supported and additional plugins should be implemented.



Building
========

  1. To build a full hop+bigloo

  make -f Makefile.android ANDROIDCC=$ANDROIDCC ANDROIDROOT=$ANDROIDROOT ANDROIDNDK=$ANDROIDROOT/android-ndk-r21b

  2. To build only hop

  make -f Makefile.android apk-sans-bigloo

  3. To build only the package

  make -f Makefile.android apk-sans-hop


See the Bigloo arch/android/README.cross.md for creating a suitable as
ANDROIDCC.


The default configuration assumes a physiscal device connected to
the USB bus. If an emulator is used, add the following to all Make invocation:

  ANDROIDHOST=-e

For additional configuration, check hop/arch/android/Makefile


Installing
==========

  adb uninstall fr.inria.hop
  adb install fr.inria.hop
  adb shell monkey -p fr.inria.hop 1


Debugging
=========

On new devices:
  adb logcat -e '[Hh]op'

On old ones:
  adb logcat | grep '[Hh]op'

To run Hop binary:
  adb exec-out run-as fr.inria.hop /data/data/fr.inria.hop/assets/bin/hop
  adb exec-out run-as fr.inria.hop /system/bin/sh -c "export HOME=/mnt/sdcard/home; export LD_LIBRARY_PATH=/data/data/fr.inria.hop/assets/lib/bigloo/4.3h:/data/data/fr.inria.hop/assets/lib/hop/3.3.0:$LD_LIBRARY_PATH;exec /data/data/fr.inria.hop/assets/bin/hop --no-color -p 8080 -g0 --max-threads 6 -z --no-jobs --rc-dir /mnt/sdcard/home/.config/hopdac -v2"


