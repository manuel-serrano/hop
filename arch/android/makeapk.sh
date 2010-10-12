#!/bin/sh
#*=====================================================================*/
#*    serrano/prgm/project/hop/2.2.x/arch/android/makeapk.sh           */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Mon Sep 27 11:21:42 2010                          */
#*    Last change :  Tue Oct 12 14:34:00 2010 (serrano)                */
#*    Copyright   :  2010 Manuel Serrano                               */
#*    -------------------------------------------------------------    */
#*    The shell script to build the .apk for Hop on Android            */
#*=====================================================================*/

#*---------------------------------------------------------------------*/
#*    Global user configuration                                        */
#*---------------------------------------------------------------------*/
HOPVERSION=2.2.0-pre3
HOPURL=http://hop.inria.fr
BIGLOOVERSION=3.4b
ANDROID=2.1

REPOSITORY=/users/serrano/prgm/distrib

ANDROIDROOT=/misc/virtual/android/r07
ANDROIDSDK=$ANDROIDROOT/android-sdk-linux_x86
ANDROIDNDK=$ANDROIDROOT/android-ndk-r4b

BIGLOO=bigloo
AFILE=bglafile
ANDROIDBIGLOOLIB=$ANDROIDROOT/local/lib/bigloo/$BIGLOOVERSION

CC=$ANDROIDROOT/bigloo$BIGLOOVERSION/arch/android/droid-gcc

PREFIX=/data/data/fr.inria.hop
INSTALLHOME=/sdcard/home

REPOSITORY=/users/serrano/prgm/distrib
if [ "$REPODIR " != " " ]; then
  REPOSITORY=$REPODIR;
fi

#*---------------------------------------------------------------------*/
#*    private configuration                                            */
#*---------------------------------------------------------------------*/
basedir=`dirname $0`
tmp=`pwd`/build.hop
android=$tmp/hop-$HOPVERSION/android

ant=/usr/share/java/apache-ant/bin/ant

branch=`echo $HOPVERSION | sed -e "s/[-].*$//g"`

makeopt=-j4

#*---------------------------------------------------------------------*/
#*    actions                                                          */
#*---------------------------------------------------------------------*/
action_untar=yes;
action_configure=yes;
action_make=yes;
action_install=yes;
action_weblets_devel=yes
action_repository=no

while : ; do
  case $1 in
    "")
      break;;

    --androidroot=*)
      ANDROIDROOT="`echo $1 | sed 's/^[^=]*=//'`";;

    --androidsdk=*)
      ANDROIDSDK="`echo $1 | sed 's/^[^=]*=//'`";;

    --androidndk=*)
      ANDROIDNDK="`echo $1 | sed 's/^[^=]*=//'`";;

    --installhome=*)
      INSTALLHOME="`echo $1 | sed 's/^[^=]*=//'`";;

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
      repository="`echo $1 | sed 's/^[^=]*=//'`";;

    -*)
      echo "*** makeapk, unknown option $1" >&2;
      echo >&2;
      echo "Usage: makeapk [options]" >&2;
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
      --link=static\
      --android \
      --android-home=$INSTALLHOME \
      --library=mail \
      --library=calendar \
      --library=text \
      --library=phone \
      --library=hopandroid) || exit 1
fi

#*---------------------------------------------------------------------*/
#*    compile                                                          */
#*---------------------------------------------------------------------*/
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
  /bin/rm $android/assets/bin/hop-$branch
  /bin/rm $android/assets/bin/hopc
  /bin/rm $android/assets/bin/hopsh
  /bin/rm $android/assets/hoplib/hop/$branch/*.so
  /bin/rm $android/assets/hoplib/hop/$branch/*.heap
  /bin/rm $android/assets/hoplib/hop/$branch/*.a
  /bin/rm $android/assets/hoplib/*.so
  
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
cat $basedir/local.properties.in $basedir/build.properties.in \
  | sed -e "s|@ANDROIDSDK@|$ANDROIDSDK|" \
        -e "s|@BASEDIR@|$basedir|" \
  > $android/local.properties || exit 1

for p in build.xml default.properties Android.mk; do \
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

# build the .apk
(cd $android &&
 $ant -Dbuild.compiler.fulldepend=true -Dbuild.compiler.compilerarg="-Xlint:unchecked" release <<EOF
hophop
hophop

EOF
if [ "$? " != "0 " ]; then
  exit 1
fi
) || exit 1

# copy the apk in the repo directory
if [ $action_repository = "yes" ]; then
  cp $android/bin/hop-release.apk $REPOSITORY/android/$ANDROID/hop-$HOPVERSION.apk
fi
