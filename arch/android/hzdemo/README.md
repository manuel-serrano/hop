Hz demo (13 Nov 2020)
=====================

This directory contains an example of an Android Hop Hz application.
That is, an example of a Hop application that must be executed on an
Android phone but "inside" an already installed Hop.

In other words, to be executed, the application needs the Hop apk to
be previously installed.


Building the APK
================

```shell
make -f Makefile.android
```
 
Installing the APK
==================

```shell
adb uninstall fr.inria.hzdemo
adb install /tmp/build.hzdemo-0.0.0/hzdemo-0.0.0.apk
adb shell monkey -p fr.inria.hzdemo 1
```

Debugging
=========

On new devices:

```shell
adb logcat '[Hh]op'
```

Access all the hzdemo resources:

```shell
adb exec-out run-as fr.inria.hzdemo whatever-command
```
