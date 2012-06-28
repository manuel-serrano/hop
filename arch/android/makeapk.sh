#!/bin/sh
#*=====================================================================*/
#*    serrano/prgm/project/hop/2.4.x/arch/android/makeapk.sh           */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Mon Sep 27 11:21:42 2010                          */
#*    Last change :  Thu Jun 28 17:36:49 2012 (serrano)                */
#*    Copyright   :  2010-12 Manuel Serrano                            */
#*    -------------------------------------------------------------    */
#*    The shell script to build the .apk for Hop on Android            */
#*=====================================================================*/

#*---------------------------------------------------------------------*/
#*    Global user configuration                                        */
#*---------------------------------------------------------------------*/
HOPVERSION=2.4.0
HOPURL=http://hop.inria.fr
BIGLOOVERSION=3.8d
ANDROID=2.1

REPOSITORY=/users/serrano/prgm/distrib

ANDROIDROOT=/misc/virtual/android/r07
ANDROIDGIT=$ANDROIDROOT/eclair-git
ANDROIDSDK=$ANDROIDROOT/android-sdk-linux_x86
ANDROIDNDK=$ANDROIDROOT/android-ndk-r4b

BIGLOO=bigloo
AFILE=bglafile
ANDROIDBIGLOOLIB=$ANDROIDROOT/local/lib/bigloo/$BIGLOOVERSION

CC=$ANDROIDROOT/bigloo$BIGLOOVERSION/arch/android/droid-gcc

PREFIX=/data/data/fr.inria.hop

REPOSITORY=/users/serrano/prgm/distrib
if [ "$REPODIR " != " " ]; then
  REPOSITORY=$REPODIR;
fi

#*---------------------------------------------------------------------*/
#*    private configuration                                            */
#*---------------------------------------------------------------------*/
basedir=`dirname $0`
tmp=`pwd`/build.hop

ant=/usr/share/apache-ant/bin/ant

if [ "$MAKEJOBS " = " " ]; then
  MAKEJOBS=2
fi

makeopt=-j$MAKEJOBS

# link should be set to "library" as soon as the GC support dynamic loading
link=static

#*---------------------------------------------------------------------*/
#*    actions                                                          */
#*---------------------------------------------------------------------*/
action_untar=yes;
action_configure=yes;
action_make=yes;
action_install=yes;
action_weblets_devel=yes
action_repository=no
action_mode=release
action_mode=debug

while : ; do
  case $1 in
    "")
      break;;

    --hopversion=*)
      HOPVERSION="`echo $1 | sed 's/^[^=]*=//'`";;

    --androidroot=*)
      ANDROIDROOT="`echo $1 | sed 's/^[^=]*=//'`";;

    --androidsdk=*)
      ANDROIDSDK="`echo $1 | sed 's/^[^=]*=//'`";;

    --androidndk=*)
      ANDROIDNDK="`echo $1 | sed 's/^[^=]*=//'`";;

    --droid-gcc=*)
      CC="`echo $1 | sed 's/^[^=]*=//'`";;

    --untar=*)
      action_untar="`echo $1 | sed 's/^[^=]*=//'`";;

    --configure=*)
      action_configure="`echo $1 | sed 's/^[^=]*=//'`";;

    --make=*)
      action_make="`echo $1 | sed 's/^[^=]*=//'`";;

    --install=*)
      action_install="`echo $1 | sed 's/^[^=]*=//'`";;

    --weblets-devel=*)
      action_weblets_devel="`echo $1 | sed 's/^[^=]*=//'`";;

    --install-repo=*)
      action_repository="`echo $1 | sed 's/^[^=]*=//'`";;

    --repository=*)
      REPOSITORY="`echo $1 | sed 's/^[^=]*=//'`";;

    --mode=*)
      action_mode="`echo $1 | sed 's/^[^=]*=//'`";;

    -*)
      echo "*** makeapk, unknown option $1" >&2;
      echo >&2;
      echo "Usage: makeapk [options]" >&2;
      echo "   --hopversion=version.... hop version to install" >&2;
      echo "   --androidroot=dir....... android root directory" >&2;
      echo "   --androidsdk=dir........ sdk path (defaults androidroot/android-sdk-linux_x86)" >&2;
      echo "   --androidndk=dir........ ndk path (defaults androidroot/android-ndk-r4b)" >&2;
      echo "   --droid-gcc=path........ gcc (defaults androidroot/biglooXXX/arch/android/droid-gcc)" >&2;
      echo "   --untar=yes|no.......... untar Hop" >&2;
      echo "   --configure=yes|no...... configure Hop" >&2;
      echo "   --make=yes|no........... make Hop" >&2;
      echo "   --install=yes|no........ install Hop" >&2;
      echo "   --install-repo=yes|no... install in the Hop repo" >&2;
      echo "   --weblets-devel=yes|no.. full weblets installations" >&2;
      echo "   --repository=dir........ output target directory" >&2;
      echo "   --mode=debug|release.... output target directory" >&2;
      echo "" >&2;
      echo "Example:" >&2;
      echo "   $0 --untar=no --make=no --install=no --weblets-devel=no" >&2;
      exit -1;;

    *)
      echo "*** ERROR: unknown option \"$1\"";
      exit 1;;
  esac
  shift
done

android=$tmp/hop-$HOPVERSION/android
branch=`echo $HOPVERSION | sed -e "s/[-].*$//g"`

#*---------------------------------------------------------------------*/
#*    Install the Hop source code                                      */
#*---------------------------------------------------------------------*/
if [ $action_untar = "yes" ]; then
  /bin/rm -rf $tmp
  mkdir -p $tmp

  tar xfz $REPOSITORY/hop-$HOPVERSION.tar.gz -C $tmp
fi

#*---------------------------------------------------------------------*/
#*    configure                                                        */
#*---------------------------------------------------------------------*/
if [ $action_configure = "yes" ]; then
  (cd $tmp/hop-$HOPVERSION && \
   ./configure \
      --bigloo=$BIGLOO \
      --bglafile=$AFILE \
      --prefix=$PREFIX \
      --libdir=$PREFIX/hoplib \
      --cc=$CC \
      --bigloolibdir=$ANDROIDBIGLOOLIB \
      --link=$link \
      --disable-ssl \
      --disable-avahi \
      --android \
      --library=mail \
      --library=calendar \
      --library=text \
      --library=phone \
      --library=hopdroid) || exit 1
fi

#*---------------------------------------------------------------------*/
#*    compile                                                          */
#*---------------------------------------------------------------------*/
mkdir -p $android

if [ $action_make = "yes" ]; then
  make $makeopt -C $tmp/hop-$HOPVERSION || exit 1
fi

if [ $action_install = "yes" ]; then
  mkdir -p $android/assets
  make -C $tmp/hop-$HOPVERSION install \
    HOPETCDIR=$android/assets/etc \
    HOPBINDIR=$android/assets/bin \
    HOPLIBDIR=$android/assets/hoplib \
    HOPSHAREDIR=$android/assets/share/hop \
    HOPMANDIR=$android/assets/man \
    HOPWEBLETSDIR=$android/assets/hoplib/hop/$branch/weblets \
    HOPCONTTRIBSDIR=$android/assets/conttribs \
    || exit 1
  
  # cleanup useless file
  if [ "$link" = "static" ]; then
    /bin/rm $android/assets/bin/hop-$branch
    /bin/rm $android/assets/bin/hopc
    /bin/rm $android/assets/bin/hopsh
    /bin/rm $android/assets/hoplib/hop/$branch/*.so
    /bin/rm $android/assets/hoplib/hop/$branch/*.heap
    /bin/rm $android/assets/hoplib/hop/$branch/*.a
    /bin/rm $android/assets/hoplib/*.so
  fi
  
  /bin/rm -rf $android/assets/man
  /bin/rm -rf $android/assets/hoplib/hop/$branch/weblets/home

  # optional devel weblets
  if [ $action_weblets_devel != "yes" ]; then
    /bin/rm -rf $android/assets/hoplib/hop/$branch/weblets/doc
    /bin/rm -rf $android/assets/hoplib/hop/$branch/weblets/wiki
    /bin/rm -rf $android/assets/hoplib/hop/$branch/weblets/hzbuilder
  fi
fi

#*---------------------------------------------------------------------*/
#*    apk build                                                        */
#*---------------------------------------------------------------------*/
for p in local.properties ant.properties; do
  cat $basedir/$p.in \
    | sed -e "s|@ANDROIDSDK@|$ANDROIDSDK|" \
          -e "s|@BASEDIR@|$basedir|" \
    > $android/$p || exit 1
done

for p in build.xml project.properties Android.mk; do \
  cp $basedir/$p $android
done

for p in AndroidManifest.xml; do \
  rm -f $android/$p.in
  cat $basedir/$p.in \
    | sed -e "s|@HOPVERSION@|$HOPVERSION|" \
          -e "s|@HOPURL@|$HOPURL|" > $android/$p
done

for p in jni src res; do \
  cp -r $basedir/$p $android
done

for p in res/values/strings.xml; do \
  rm -f $android/$p.in
  cat $basedir/$p.in \
    | sed -e "s|@HOPVERSION@|$HOPVERSION|" \
          -e "s|@HOPURL@|$HOPURL|" > $android/$p
done

# the apkbuilder ignores the .afiles, so we change their names
# and the installer changes them back
for afile in `find $android/assets -name .afile -print`; do
  # dot.afile
  dot_afile=`dirname $afile`/dot`basename $afile`
  mv $afile $dot_afile
done

# rename .js.gz into .jsgz (otherwise ANT complains)
for jsgz in $android/assets/share/hop/*.js.gz; do
  base=`basename $jsgz .js.gz`

  if [ "$base " !=  "* " ]; then
    mv $jsgz $android/assets/share/hop/$base.jsgz
  fi
done

$ANDROIDNDK/ndk-build -C $android V=1 || exit 1

# build the jmdns jar file
(cd $tmp/hop-$HOPVERSION/arch/android && 
 mkdir build && 
 javac -d build javax/jmdns/JmmDNS.java && 
 cd build && jar cf jmdns.jar javax && cd .. &&
 $ANDROIDGIT/out/host/linux-x86/bin/dx --dex \
   --output=$android/assets/hoplib/jmdns.jar \
   build/jmdns.jar)

# cleanup old apk
for p in $tmp/hop-$HOPVERSION/android/bin/*.apk; do
    /bin/rm -f $p
done

# build the .apk
(cd $android &&
 $ant -Dbuild.compiler.fulldepend=true -Dbuild.compiler.compilerarg="-Xlint:unchecked" $action_mode <<EOF
hophop
hophop

EOF
if [ "$? " != "0 " ]; then
  exit 1
fi
) || exit 1

# copy the apk in the repo directory
if [ $action_repository = "yes" ]; then
  cp $android/bin/hop-$action_mode.apk $REPOSITORY/android/hop-$HOPVERSION.apk
fi
