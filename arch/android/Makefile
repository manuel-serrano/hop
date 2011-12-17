#*=====================================================================*/
#*    serrano/prgm/project/hop/2.3.x/arch/android/Makefile             */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Tue Oct 12 11:53:03 2010                          */
#*    Last change :  Sat Dec 17 07:08:10 2011 (serrano)                */
#*    Copyright   :  2010-11 Manuel Serrano                            */
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
ANDROID_BUILD = Android.mk build.xml project.properties hop.keystore \
  AndroidManifest.xml.in local.properties.in and.properties.in proguard.cfg

ANDROID_JNI = jni/Android.mk jni/termExec.cpp

ANDROID_RES = res/drawable/logo.png \
  res/layout/main.xml \
  res/xml/preferences.xml \
  res/values/hopstyle.xml res/values/arrays.xml res/values/strings.xml.in

ANDROID_JAVA = Hop.java HopConfigurer.java HopInstaller.java HopUiUtils.java \
  HopExec.java HopLauncher.java \
  HopDroid.java HopPlugin.java HopPluginInit.java HopPluginLocale.java \
  HopPluginMusicPlayer.java HopPluginVibrate.java HopPluginSensor.java \
  HopPluginSms.java HopPluginContact.java HopPluginWifi.java \
  HopPluginBattery.java HopPluginTts.java HopPluginMediaAudio.java \
  HopPluginCall.java HopSms.java HopAndroidAudioRecorder.java 

#*---------------------------------------------------------------------*/
#*    Population                                                       */
#*---------------------------------------------------------------------*/
POPULATION = Makefile makeapk.sh \
  $(ANDROID_BUILD) $(ANDROID_JNI) $(ANDROID_RES) \
  $(ANDROID_JAVA:%=src/fr/inria/hop/%)

POPDIRS = hopdroid androidemo

#*---------------------------------------------------------------------*/
#*    build                                                            */
#*---------------------------------------------------------------------*/
build:
	$(MAKE) -C hopdroid build
	$(MAKE) -C androidemo build

install:
	$(MAKE) -C hopdroid install
	$(MAKE) install-weblet DIR=androidemo

uninstall:
	$(MAKE) -C hopdroid uninstall

#*---------------------------------------------------------------------*/
#*    install-weblet                                                   */
#*---------------------------------------------------------------------*/
install-weblet:
	if [ ! -d $(DESTDIR)$(HOPWEBLETSDIR)/$(DIR) ]; then \
	   mkdir -p $(DESTDIR)$(HOPWEBLETSDIR)/$(DIR) && \
              chmod $(MODDIR) $(DESTDIR)$(HOPWEBLETSDIR)/$(DIR); \
        fi
	for f in $(DIR)/*; do \
          if [ -d $$f ]; then \
            $(MAKE) install-weblet DIR=$$f || exit 1; \
          else \
            if [ -f $$f ]; then \
	      $(INSTALL) $$f $(DESTDIR)$(HOPWEBLETSDIR)/$(DIR) \
                && chmod $(MODFILE) $(DESTDIR)$(HOPWEBLETSDIR)/$$f \
                || exit 1; \
            fi; \
          fi; \
        done; \
        if [ -f $(DIR)/.afile ]; then \
          $(INSTALL) $(DIR)/.afile $(DESTDIR)$(HOPWEBLETSDIR)/$(DIR) \
            && chmod $(MODFILE) $(DESTDIR)$(HOPWEBLETSDIR)/$(DIR)/.afile \
            || exit 1; \
        fi; \

