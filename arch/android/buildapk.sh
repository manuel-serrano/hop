#!/bin/sh
#*=====================================================================*/
#*    serrano/prgm/project/hop/hop/arch/android/buildapk.sh            */
#*    -------------------------------------------------------------    */
#*    Author      :  manuel serrano                                    */
#*    Creation    :  Wed May 13 18:51:59 2020                          */
#*    Last change :  Wed May 20 08:38:53 2020 (serrano)                */
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

androidkeystore=$HOME/.android/debug.keystore

libdir=lib

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

    --androidkeystore=*)
      androidkeystore="`echo $1 | sed 's/^[^=]*=//'`";;

    --javac=*)
      javac="`echo $1 | sed 's/^[^=]*=//'`";;

    --jarsigner=*)
      jarsigner="`echo $1 | sed 's/^[^=]*=//'`";;

    --nolibdir)
      libdir=;;
    
    *)
      apkname=$1;;
  esac
  shift
done

if [ "$ANDROIDBUILDTOOLSVERSION " = " " ]; then
  ANDROIDBUILDTOOLSVERSION=`ls $ANDROIDSDK/build-tools | tail -n 1 | sed 's|/||'` || exit 1
fi

if [ "$ANDROIDPLATFORM " = " " ]; then
  ANDROIDPLATFORM=`ls $ANDROIDSDK/platforms | tail -n 1 | sed 's|/||'` || exit 1
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
echo "$AAPT package -f -m -J src -M AndroidManifest.xml -S res -I $ANDROIDCP"
$AAPT package -f -m -J src -M AndroidManifest.xml -S res -I $ANDROIDCP || exit 1

mkdir -p bin

echo "$javac -classpath $ANDROIDCP -sourcepath 'src' -d 'bin' `find src -name "*.java"`"
$javac -classpath $ANDROIDCP -sourcepath 'src' -d 'bin' `find src -name "*.java"`  || exit 1

echo "$DX --dex --output=classes.dex bin"
$DX --dex --output=classes.dex bin || exit 1

/bin/rm -f $apkname.apk.unaligned 
echo "$AAPT package -f -M AndroidManifest.xml -S res -I $ANDROIDCP -F $apkname.apk.unaligned "
$AAPT package -f -M AndroidManifest.xml -S res -I $ANDROIDCP -F $apkname.apk.unaligned || exit 1

echo "$AAPT add $apkname.apk.unaligned classes.dex"
$AAPT add $apkname.apk.unaligned classes.dex || exit 1

rm -rf lib

if [ "$libdir " = " " ]; then
  rm -rf libs
else  
  mv libs $libdir
  
  for p in `find $libdir -type f -print`; do
    $AAPT add $apkname.apk.unaligned $p
  done
fi  

for p in `find assets -type f -print`; do
  $AAPT add $apkname.apk.unaligned $p
done

echo "jarsigner -keystore $androidkeystore -storepass 'android' $apkname.apk.unaligned androiddebugkey"
jarsigner -keystore $androidkeystore -storepass 'android' $apkname.apk.unaligned androiddebugkey || exit 1

/bin/rm -f $apkname.apk
echo "$ZIPALIGN -f 4 $apkname.apk.unaligned $apkname.apk"
$ZIPALIGN -f 4 $apkname.apk.unaligned $apkname.apk || exit 1

/bin/rm -f $apkname.apk.unaligned

echo "done...$apkname.apk"
