Building
========

  1. To build a full hop+bigloo

  make -f Makefile.android
  
or
  
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
  make -f Makefile.android install.apk
  make -f Makefile.android click.apk


Debugging
=========

On new devices:
  adb logcat -e '[Hh]op'

On old ones:
  adb logcat | grep '[Hh]op'

To run Hop binary:
  adb exec-out run-as fr.inria.hop /data/data/fr.inria.hop/assets/bin/hop
