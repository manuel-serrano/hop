#! /bin/bash

if [ "$(basename $0)" == "build-android.sh" ]; then
  set -e
fi

# root for all things android
ANDROIDROOT=$HOME/src/works/inria/android
# ANDROIDROOT=/misc/virtual/android

export ANDSRC=$ANDROIDROOT/eclair-git
export ANDSDK=$ANDROIDROOT/android-sdk-linux

# we can't fire an emulator automatically, so just do it yourself by hand
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

# the bootstraping bigloo libdir dir
export BGL_LIBDIR=$HOME/local/soft/bigloo-android/lib/bigloo/3.3b
# the bootstraping hop dir
export BS_HOPDIR=$HOME/src/works/inria/bootstrap/hop-live

prefix=/data/data/fr.inria.hop
install_prefix=$(pwd)/arch/android/assets
# libdir=$prefix/libs/armeabi
libdir=$install_prefix/lib

if [ "$1" == "configure" ]; then
  ./configure --disable-threads \
    --prefix=$prefix \
    --cc=$CC \
    --bigloolibdir=$BGL_LIBDIR
  shift
fi

if [ "$1" == "build" ]; then
  # yes, we need a bootstraping hop too :|
  # unluckily our build system is not ready for this kind of things
  # so just hack it away
  pwd=$(pwd)
  ( cd widget
    for i in *.hop; do
      $BS_HOPDIR/bin/hopc $i -o o/${i%.hop}.o -c \
        --bigloo=bigloo -L $BS_HOPDIR/lib \
        --share-dir $pwd/share -- \
        -O2 -fsharing -Wall -wslots -L $BS_HOPDIR/lib \
        -lib-dir $BGL_LIBDIR \
        -cc $CC \
        -copt "-O3 -DPLATFORM_ANDROID -I$BGL_LIBDIR" \
        -copt -fPIC -unsafe -safee
    done
  )
  make
  shift
fi

function install-prefix {
    # installs $file in $prefix/$file,
    # creating all the parent dirs as needed
    file="$1"
    prefix="$2"

    dir="$(dirname $file)"
    mkdir -p "$prefix/$dir"
    cp -v "$file" "$prefix/$dir"
}

function install {
    # installs $src in $dst,
    # creating all the parent dirs as needed
    src="$1"
    dst="$2"

    dir="$(dirname $dst)"
    mkdir -p "$dir"
    cp -vr "$src" "$dst"
}

if [ "$1" == "apk" ]; then
  # make install
  # we have to install by hand because prefix is needed for the host layout
  rm -rf $install_prefix
  for file in bin/hop etc/hoprc.hop lib/*.{so,init}; do
    install "$file" "$install_prefix/$file"
  done
  for file in share/*; do
    install "$file" "$install_prefix/share/hop/$(basename $file)"
  done


  # we have to massage a little the output of 'make install' befoe we build the .apk
  # FIX: [null] Unable to add '/home/mdione/src/works/inria/android/live/hop-2.0.0-android-basic/android/assets/share/hop/base64.js.gz': file already in archive (try '-u'?)
  # rm -rf $install_prefix/share

  # FIX: copy the hop binary in the Makefiles
  # cp -v bin/hop $prefix/assets/bin
  # remove symlinks and move .so/.init files to lib
  # rm -fv $libdir/*.so
  (
    . ./.hoprelease
    # mv -v $libdir/hop/$major/*_{s,e}-$major.so $libdir
    # mkdir -p $libdir
    # mv -v $libdir/hop/$major/*.init $libdir
  )

  # FIX: V/hop-installer(  288): Error: /data/data/fr.inria.hop/lib/hop/2.1.0/weblets/color/etc/color.wiki
  # rm -rf $libdir/hop

  # throw in bigloo's libs too
  cp -v $BGL_LIBDIR/libbigloo{gc,{,web,multimedia}_s}*.so $libdir
  cp -v $BGL_LIBDIR/libbigloo{web,multimedia}_e*.so $libdir
  cp -v $BGL_LIBDIR/*.init $libdir

  (
    cd arch/android
    # finally build the .apk
    ant debug
  )
  shift
fi

if [ "$1" == "install" ]; then
   $ANDSDK/tools/adb install -r arch/android/bin/hop-debug.apk
   shift
fi

if [ "$1" == "unpack" ]; then
   $ANDSDK/tools/adb shell monkey -p fr.inria.hop 1
   shift
fi
