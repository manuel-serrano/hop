#! /bin/bash

if [ "$(basename $0)" == "build-android.sh" ]; then
  set -e
fi

# root for all things android
ANDROIDROOT=$HOME/src/works/inria/android
# ANDROIDROOT=/misc/virtual/android

export ANDSRC=$ANDROIDROOT/eclair-git
export ANDSDK=$ANDROIDROOT/android-sdk-linux

# try to communicate with the emulator, or fire it if is not there.
# we can't fire one automatically, so just do it yourself by hand
export ANDROID_SERIAL="emulator-5556"

# droid-wrapper
# http://github.com/tmurakam/droid-wrapper/
export DROID_ROOT=$ANDSRC
# 3 for cupcake
# 5 for eclair
export DROID_TARGET=5

# bigloo/gc
export CC=$ANDROIDROOT/droid-wrapper/bin/droid-gcc

# so ant works :|
export JAVA_HOME=/usr/lib/jvm/java-6-sun-1.6.0.17

BGL_LIBDIR=$HOME/local/soft/bigloo-android/lib/bigloo/3.3b

prefix=$(pwd)/arch/android

if [ "$1" == "configure" ]; then
  ./configure --disable-threads \
    --prefix=$prefix/assets --libdir=$prefix/libs/armeabi \
    --cc=$CC \
    --bigloolibdir=$BGL_LIBDIR
  shift
fi

if [ "$1" == "build" ]; then
  # nice -n 19 make -j 8
  # nice -n 19 make --print-data-base --warn-undefined-variables
  make
  shift
fi

if [ "$1" == "apk" ]; then
  make install
  (
    cd arch/android
    # FIX: [null] Unable to add '/home/mdione/src/works/inria/android/live/hop-2.0.0-android-basic/android/assets/share/hop/base64.js.gz': file already in archive (try '-u'?)
    rm -rf assets/share
    # FIX: copy the hop binary in the Makefiles
    cp -v ../../bin/hop assets/bin
    # copy bigloo's libs too
    cp -v $BGL_LIBDIR/libbigloo{gc,{,web,multimedia}_s}*.so $prefix/libs/armeabi/
    ant debug
  )
  shift
fi

if [ "$1" == "install" ]; then
   $ANDSDK/tools/adb install -r arch/android/bin/hop-debug.apk
   shift
fi
