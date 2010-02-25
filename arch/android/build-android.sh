#! /bin/bash

if [ "$(basename $0)" == "build-android.sh" ]; then
  set -e
fi

# root for all things android
ANDROIDROOT=$HOME/src/works/inria/android
# ANDROIDROOT=/misc/virtual/android

export ANDSRC=$ANDROIDROOT/eclair-git
export ANDSDK=$ANDROIDROOT/android-sdk-linux_86

# we can't fire an emulator automatically, so just do it yourself by hand
export ANDROID_SERIAL="emulator-5554"
# export ANDROID_SERIAL="emulator-5556"

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
libdir=$install_prefix/lib

# weblets="wizard,hop,hz,shutdown,info"

function install {
    # installs $src in $dst,
    # creating all the parent dirs as needed
    src="$1"
    dst="$2"

    dir="$(dirname $dst)"
    mkdir -p "$dir"
    cp -vr "$src" "$dst"
}

if [ "$1" == "configure" -o "$1" == "all" ]; then
  ./configure --disable-threads \
    --prefix=$prefix \
    --libdir=$prefix/lib \
    --cc=$CC \
    --bigloolibdir=$BGL_LIBDIR

  if [ "$1" == "configure" ]; then
    shift
  fi
fi

if [ "$1" == "build" -o "$1" == "all" ]; then
  # yes, we need a bootstraping hop too :|
  # unluckily our build system is not ready for this kind of things
  # so just hack it away
  pwd=$(pwd)

  # this first attempt to build will fail because the widget/ directory will fail
  # but it will be enough to actually build it by hand
  # and the the latter make there below will continue the building
  make || true
  ( cd widget
    for i in *.hop; do
      echo $BS_HOPDIR/bin/hopc $i -o o/${i%.hop}.o -c
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

  if [ "$1" == "build" ]; then
    shift
  fi
fi

if [ "$1" == "apk" -o "$1" == "all" ]; then
  # we have to install by hand because prefix is needed for the host layout
  source .hoprelease
  rm -rf $install_prefix
  for file in bin/hop etc/hoprc.hop lib/*.{so,init}; do
    install "$file" "$install_prefix/$file"
  done
  for file in share/{buttons,icons,*.js,*.hss,*.scm,.afile}; do
    install "$file" "$install_prefix/share/hop/$(basename $file)"
  done
  # don't install all the weblets
  for file in weblets/{wizard,hop,hz,shutdown,info}; do
    install "$file" "$install_prefix/lib/hop/$major/weblets/$(basename $file)"
  done

  # throw in bigloo's libs too
  cp -v $BGL_LIBDIR/libbigloo{gc,{,web,multimedia}_s}*.so $libdir
  cp -v $BGL_LIBDIR/libbigloo{web,multimedia}_e*.so $libdir
  cp -v $BGL_LIBDIR/*.init $libdir

  (
    cd arch/android
    ant package-resources
    # aaand add .afiles because the apk builder skips them
    # zip -u bin/hop-debug.apk assets/lib/hop/$major/weblets/{wizard,hop,hz,shutdown,info}/.afile
    find assets -name .afile | xargs zip -u bin/hop-debug.apk
    # finally build the .apk
    ant debug
  )

  if [ "$1" == "apk" ]; then
    shift
  fi
fi

if [ "$1" == "install" -o "$1" == "all" ]; then
  $ANDSDK/tools/adb install -r arch/android/bin/hop-debug.apk

  if [ "$1" == "install" ]; then
    shift
  fi
fi

if [ "$1" == "unpack" -o "$1" == "all" ]; then
  $ANDSDK/tools/adb shell monkey -p fr.inria.hop 1
  ( cd arch/android/assets
    find . -name .afile | while read file; do
      $ANDSDK/tools/adb push $file /data/data/fr.inria.hop/$file
    done
  )
  if [ "$1" == "unpack" ]; then
    shift
  fi
fi
