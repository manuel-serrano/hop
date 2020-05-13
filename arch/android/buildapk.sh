#!/bin/sh
#*=====================================================================*/
#*    serrano/prgm/project/hop/hop/arch/android/buildapk.sh            */
#*    -------------------------------------------------------------    */
#*    Author      :  manuel serrano                                    */
#*    Creation    :  Wed May 13 18:51:59 2020                          */
#*    Last change :  Wed May 13 19:10:19 2020 (serrano)                */
#*    Copyright   :  2020 manuel serrano                               */
#*    -------------------------------------------------------------    */
#*    build the Android APK after ndk-build                            */
#*    -------------------------------------------------------------    */
#*    This is a substitute for ant, gradle, or cmake.                  */
#*---------------------------------------------------------------------*/

#*---------------------------------------------------------------------*/
#*    Android sdk directory                                            */
#*---------------------------------------------------------------------*/
ANDROIDROOT=/misc/virtual/android
ANDROIDNDK=$ANDROIDROOT/android-ndk-r21b
ANDROIDSDK=$ANDROIDROOT/android-sdk-linux
ANDROIDBUILDTOOLSVERSION=
ANDROIDPLATFORM=
ANDROIDCP=

javac=javac
jarsigner=jarsigner

#*---------------------------------------------------------------------*/
#*    argument parsing                                                 */
#*---------------------------------------------------------------------*/
while : ; do
  case $1 in
    "")
      break;;

    --androidsdk=*)
      ANDROIDSDK="`echo $1 | sed 's/^[^=]*=//'`";;

    --androidndk=*)
      ANDROIDNDK="`echo $1 | sed 's/^[^=]*=//'`";;

    --androidbuildtoolsversion=*)
      ANDROIDBUILDTOOLSVERSION="`echo $1 | sed 's/^[^=]*=//'`";;

    --androidplatform=*)
      ANDROIDPLATFORM="`echo $1 | sed 's/^[^=]*=//'`";;

    --androidcp=*)
      ANDROIDCP="`echo $1 | sed 's/^[^=]*=//'`";;

    --javac=*)
      javac="`echo $1 | sed 's/^[^=]*=//'`";;

    --jarsigner=*)
      jarsigner="`echo $1 | sed 's/^[^=]*=//'`";;

    *)
      apkname=$1;;
  esac
  shift
done

if [ "$ANDROIDBUILDTOOLSVERSION " = " " ]; then
  ANDROIDBUILDTOOLSVERSION=`ls $ANDROIDSDK/build-tools | tail -n 1 | sed 's|/||'`
fi

if [ "$ANDROIDPLATFORM " = " " ]; then
  ANDROIDPLATFORM=`ls $ANDROIDSDK/platform | tail -n 1 | sed 's|/||'`
fi

if [ "$ANDROIDCP " = " " ]; then
  ANDROIDCP=$ANDROIDSDK/platforms/$ANDROIDPLATFORM/android.jar
fi  

#*---------------------------------------------------------------------*/
#*    Android tools                                                    */
#*---------------------------------------------------------------------*/
DX=$ANDROIDSDK/build-tools/$ANDROIDBUILDTOOLSVERSION/dx
AAPT=$ANDROIDSDK/build-tools/$ANDROIDBUILDTOOLSVERSION/aapt
ZIPALIGN=$ANDROIDSDK/build-tools/$ANDROIDBUILDTOOLSVERSION/zipalign

#*---------------------------------------------------------------------*/
#*    Apk production                                                   */
#*---------------------------------------------------------------------*/
$AAPT package -f -m -J src -M AndroidManifest.xml -S res -I $ANDROIDCP

$javac -classpath $ANDROIDCP -sourcepath 'src' -d 'bin' -target 1.7 -source 1.7 `find src -name "*.java"` 

$DX --dex --output=classes.dex bin

/bin/rm -f $apkname.apk.unaligned 
$AAPT package -f -M AndroidManifest.xml -S res -I $ANDROIDCP -F $apkname.apk.unaligned 
$AAPT add $apkname.apk.unaligned classes.dex

rm -rf lib
cp -r libs lib

for p in `find lib -type f -print`; do
  $AAPT add $APKNAME.apk.unaligned $p
done

for p in `find assets -type f -print`; do
  $AAPT add $APKNAME.apk.unaligned $p
done

jarsigner -keystore ~/.android/debug.keystore -storepass 'android' $apkname.apk.unaligned androiddebugkey

/bin/rm -f $apkname.apk
$ZIPALIGN -f 4 $apkname.apk.unaligned $apkname.apk
