Requirements
============



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


