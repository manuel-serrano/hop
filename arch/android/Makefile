#*=====================================================================*/
#*    serrano/prgm/project/hop/2.2.x/arch/android/Makefile             */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Tue Oct 12 11:53:03 2010                          */
#*    Last change :  Tue Oct 12 14:03:18 2010 (serrano)                */
#*    Copyright   :  2010 Manuel Serrano                               */
#*    -------------------------------------------------------------    */
#*    Android port                                                     */
#*=====================================================================*/

do: build

#*---------------------------------------------------------------------*/
#*    Configuration                                                    */
#*---------------------------------------------------------------------*/
-include ../../etc/Makefile.hopconfig
-include ../../etc/Makefile.version

#*---------------------------------------------------------------------*/
#*    Sources                                                          */
#*---------------------------------------------------------------------*/
ANDROID_BUILD = Android.mk build.xml default.properties hop.keystore \
  AndroidManifest.xml.in local.properties.in build.properties.in

ANDROID_JNI = jni/Android.mk jni/termExec.cpp

ANDROID_RES = res/drawable/logo.png \
  res/layout/main.xml \
  res/values/hopstyle.xml res/values/strings.xml.in

ANDROID_JAVA = Hop.java HopConfigurer.java HopInstaller.java HopUiUtils.java \
  HopAndroid.java HopExec.java HopLauncher.java

#*---------------------------------------------------------------------*/
#*    Population                                                       */
#*---------------------------------------------------------------------*/
POPULATION = Makefile makeapk.sh \
  $(ANDROID_BUILD) $(ANDROID_JNI) $(ANDROID_RES) \
  $(ANDROID_JAVA:%=src/fr/inria/hop/%) \

POPDIRS = hopandroid

#*---------------------------------------------------------------------*/
#*    build                                                            */
#*---------------------------------------------------------------------*/
build:
	$(MAKE) -C hopandroid build

install:
	$(MAKE) -C hopandroid install
