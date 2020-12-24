#*=====================================================================*/
#*    serrano/prgm/project/hop/hop/arch/android/Makefile               */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Tue Oct 12 11:53:03 2010                          */
#*    Last change :  Wed May 27 16:19:08 2020 (serrano)                */
#*    Copyright   :  2010-20 Manuel Serrano                            */
#*    -------------------------------------------------------------    */
#*    Android port                                                     */
#*    -------------------------------------------------------------    */
#*    Tested May 2020 with Android ndk r21b                            */
#*=====================================================================*/

do: build

#*---------------------------------------------------------------------*/
#*    Configuration                                                    */
#*---------------------------------------------------------------------*/
-include ../../etc/Makefile.hopconfig
-include ../../etc/Makefile.version

#*---------------------------------------------------------------------*/
#*    Configuration                                                    */
#*---------------------------------------------------------------------*/
ANDROIDBUILDDIR = $(HOPTMPDIR)/build.$(HOPAPKNAME).android
ANDROIDACTIONMODE = debug
ANDROIDHOST = -d
ANDROIDDIR= $(BUILDDIR)/arch/android
ANDROIDKEYSTORE = $(ANDROIDDIR)/hop.keystore
ANDROIDTARGET = armv7a-linux-androideabi26
ANDROIDSDKVERSION = 21

ANDROIDLINK = dynamic

#*---------------------------------------------------------------------*/
#*    Android sdk                                                      */
#*---------------------------------------------------------------------*/
ANDROIDSDK = $(shell echo "$$ANDROIDSDK")
ANDROIDNDK = $(shell echo "$$ANDROIDNDK")

ANDROIDCC=$(ANDROIDNDK)/toolchains/llvm/prebuilt/linux-$(shell uname -m)/bin/clang

#*---------------------------------------------------------------------*/
#*    HOPSOURCE                                                        */
#*---------------------------------------------------------------------*/
HOPSOURCE = $(REPOSITORY)/$(HOPFULLNAME).tar.gz
HOPSOURCE = $(BUILDDIR)

#*---------------------------------------------------------------------*/
#*    hopdroid configuration                                           */
#*---------------------------------------------------------------------*/
ANDROIDHOPPORT = 8080
ANDROIDHOPTHREADS = 6
ANDROIDHOPZEROCONF = true
ANDROIDHOPDEBUG = true
ANDROIDHOPURL = http://hop.inria.fr
ANDROIDHOPCONSOLELOG = false
ANDROIDHOPDEBUGLEVEL = # -g0
ANDROIDHOPVERBOSELEVEL = -v
ANDROIDHOPPREFIX = /data/data/fr.inria.$(HOPAPP)/assets

ANDROIDWEBLET =
ANDROIDMODULE =
ANDROIDRES = $(ANDROIDDIR)/res
ANDROIDMANIFEST = $(ANDROIDDIR)/AndroidManifest.xml
ANDROIDSPLASH = $(ANDROIDDIR)/splash

# Config
DEFLANG = javascript
NOTITLE = true
CUSTOMTITLE = false
STATUSBARCOLOR = default

# Plugins
PLUGINBUILD = true
PLUGINLOCALE = true
PLUGINVIBRATE = true
PLUGINMUSICPLAYER = true
PLUGINMEDIAAUDIO = true
PLUGINSENSOR = true
PLUGINBATTERY = true
PLUGINSMS = true
PLUGINWIFI = true
PLUGINCONNECTIVITY = true
PLUGINCONTACT = true
PLUGINZEROCONF = true
PLUGINSYSTEM = true
PLUGINTTS = false
PLUGINCALL = true
PLUGINPREFS = true

APKCUSTOM =
APKCUSTOMDIR =

# global configuration
REPOSITORY = $(shell if [ "$$REPODIR " = " " ]; then echo $(DISTRIBDIR); else echo $$REPODIR; fi)

HOPAPKNAME = hop
HOPAPP = hop
HOPARGS = 
HOPMINOR = -$(HOPDEVEL)1
HOPFULLNAME = hop-$(HOPRELEASE)$(HOPMINOR)
HOPAPK = fr.inria.hop

# only for client
HOPHZ = only-for-client

JAVAC = javac -target 1.7 -source 1.7 

# list of weblets to be removed before device installation
ANDROID_UNUSED_WEBLETS = color doc home hzbuilder test trace webfilter weblets wiki workbench

include $(CONFIG)

# weblets should invoke this makefile with command line such as
# $(MAKE) -C $(HOPDIR)/arch/android $(MAKECMDGOALS) CONFIG=$(HOPDACDIR)/Makefile.android
config:
	@echo "hopapkname=$(HOPAPKNAME)"
	@echo "hopapp=$(HOPAPP)"
	@echo "weblet=$(ANDROIDWEBLET)"
	@echo "module=$(ANDROIDMODULE)"
	@echo "androidManifest=$(ANDROIDMANIFEST)"
	@echo "res=$(ANDROIDRES)"
	@echo ""
	@echo "androidsdk=$(ANDROIDSDK)"
	@echo "androidndk=$(ANDROIDNDK)"
	@echo ""
	@echo "androidlink=$(ANDROIDLINK)"
	@echo ""
	@echo "pluginbuild=$(PLUGINBUILD)"
	@echo "pluginlocale=$(PLUGINLOCALE)"
	@echo "pluginvibrate=$(PLUGINVIBRATE)"
	@echo "pluginmusicplayer=$(PLUGINMUSICPLAYER)"
	@echo "pluginmediaaudio=$(PLUGINMEDIAAUDIO)"
	@echo "pluginsensor=$(PLUGINSENSOR)"
	@echo "pluginbattery=$(PLUGINBATTERY)"
	@echo "pluginsms=$(PLUGINSMS)"
	@echo "pluginwifi=$(PLUGINWIFI)"
	@echo "pluginconnectivity=$(PLUGINCONNECTIVITY)"
	@echo "plugincontact=$(PLUGINCONTACT)"
	@echo "pluginzeroconf=$(PLUGINZEROCONF)"
	@echo "pluginsystem=$(PLUGINSYSTEM)"
	@echo "plugintts=$(PLUGINTTS)"
	@echo "plugincall=$(PLUGINCALL)"
	@echo "pluginprefs=$(PLUGINPREFS)"

#*---------------------------------------------------------------------*/
#*    Sources                                                          */
#*---------------------------------------------------------------------*/
ANDROID_BUILD = Android.mk.in project.properties.in hop.keystore \
  AndroidManifest.xml.in proguard.cfg \
  README.md

ANDROID_JNI = jni/Android.mk jni/termExec.cpp

ANDROID_RES = res/drawable/logo.png \
  res/drawable/ic_menu_block.png \
  res/drawable/ic_menu_clear_playlist.png \
  res/drawable/ic_menu_info_details.png \
  res/drawable/ic_menu_preferences.png \
  res/drawable/ic_menu_refresh.png \
  res/drawable/ic_menu_stop.png \
  res/drawable/ic_menu_goto.png \
  res/drawable/logo128.png \
  res/drawable/hopicon.png \
  res/drawable-v8/favicon.png \
  res/drawable-v8/hopicon.png \
  res/drawable-v8/ic_menu_block.png \
  res/drawable-v8/ic_menu_clear_playlist.png \
  res/drawable-v8/ic_menu_info_details.png \
  res/drawable-v8/ic_menu_preferences.png \
  res/drawable-v8/ic_menu_refresh.png \
  res/drawable-v8/ic_menu_stop.png \
  res/drawable-v8/ic_menu_goto.png \
  res/drawable-v8/logo.png \
  res/drawable-v8/logo128.png \
  res/drawable-v10/favicon.png \
  res/drawable-v10/hopicon.png \
  res/drawable-v10/ic_menu_block.png \
  res/drawable-v10/ic_menu_clear_playlist.png \
  res/drawable-v10/ic_menu_info_details.png \
  res/drawable-v10/ic_menu_preferences.png \
  res/drawable-v10/ic_menu_refresh.png \
  res/drawable-v10/ic_menu_stop.png \
  res/drawable-v10/ic_menu_goto.png \
  res/drawable-v10/logo.png \
  res/drawable-v10/logo128.png \
  res/drawable-hdpi/logo.png \
  res/drawable-xhdpi/logo.png \
  res/drawable-xxhdpi/logo.png \
  res/drawable-xxxhdpi/logo.png \
  res/drawable-web/logo.png \
  res/layout/main.xml \
  res/layout/about.xml \
  res/layout/info.xml \
  res/layout/web.xml \
  res/layout-sw720dp/info.xml \
  res/layout-sw720dp/main.xml \
  res/layout-sw720dp/about.xml \
  res/layout-sw600dp/info.xml \
  res/layout-sw600dp/main.xml \
  res/layout-sw600dp/about.xml \
  res/layout-sw480dp/info.xml \
  res/layout-sw480dp/main.xml \
  res/layout-sw480dp/about.xml \
  res/xml/preferences.xml.in \
  res/values/hopstyle.xml res/values/arrays.xml res/values/strings.xml.in \
  res/values/plugins.xml.in res/values/config.xml.in res/menu/hop_menu.xml

ANDROID_JAVA = Hop.java HopConfigurer.java HopInstaller.java \
  HopUiUtils.java HopExec.java HopLauncher.java HopService.java \
  HopBootReceiver.java HopDroid.java HopPlugin.java HopPluginInit.java \
  HopPluginLocale.java HopPluginMusicPlayer.java HopPluginVibrate.java \
  HopPluginSensor.java HopPluginSms.java HopPluginContact.java \
  HopPluginWifi.java HopPluginBattery.java HopPluginTts.java \
  HopPluginMediaAudio.java HopPluginCall.java HopSms.java HopPluginPrefs.java \
  HopAndroidAudioRecorder.java HopPluginBuild.java HopPluginConnectivity.java \
  HopSettings.java HopPluginZeroconf.java HopZeroconf.java HopJmDns.java \
  HopNsdManager.java HopPluginSystem.java HopStage.java HopIntenter.java \
  HopConfig.java HopHzLauncher.java HopUtils.java HopConfiguration.java \
  HopHzService.java HopIntent.java

JMDNS_JAVA = JmDNS.java ServiceInfo.java JmmDNS.java ServiceListener.java \
  NetworkTopologyDiscovery.java ServiceTypeListener.java \
  NetworkTopologyEvent.java NetworkTopologyListener.java package-info.java \
  ServiceEvent.java

JMDNS_IMPL_JAVA = DNSCache.java JmmDNSImpl.java DNSEntry.java \
  ListenerStatus.java DNSIncoming.java NameRegister.java \
  DNSListener.java NetworkTopologyDiscoveryImpl.java DNSMessage.java \
  NetworkTopologyEventImpl.java DNSOutgoing.java ServiceEventImpl.java \
  DNSQuestion.java ServiceInfoImpl.java DNSRecord.java SocketListener.java \
  DNSStatefulObject.java DNSTaskStarter.java package-info.java HostInfo.java \
  JmDNSImpl.java

JMDNS_CNSTS_JAVA = DNSConstants.java DNSOptionCode.java DNSResultCode.java \
  DNSLabel.java DNSRecordClass.java DNSState.java \
  DNSOperationCode.java DNSRecordType.java package-info.java \

JMDNS_TASKS_JAVA = DNSTask.java Responder.java RecordReaper.java \
  package-info.java

JMDNS_RESOLVER_JAVA = DNSResolverTask.java ServiceResolver.java \
  package-info.java ServiceInfoResolver.java TypeResolver.java

JMDNS_STATE_JAVA = Announcer.java DNSStateTask.java Renewer.java \
  Canceler.java Prober.java package-info.java

ANDROID_JMDNS_JAVA = $(JMDNS_JAVA) \
  $(JMDNS_IMPL_JAVA:%=impl/%) \
  $(JMDNS_CNSTS_JAVA:%=impl/constants/%) \
  $(JMDNS_TASKS_JAVA:%=impl/tasks/%) \
  $(JMDNS_RESOLVER_JAVA:%=impl/tasks/resolver/%) \
  $(JMDNS_STATE_JAVA:%=impl/tasks/state/%)

#*---------------------------------------------------------------------*/
#*    Population                                                       */
#*---------------------------------------------------------------------*/
POPULATION = Makefile makeapk.sh.in buildapk.sh \
  $(ANDROID_BUILD) $(ANDROID_JNI) $(ANDROID_RES) \
  $(ANDROID_JAVA:%=src/fr/inria/hop/%) \
  $(ANDROID_JMDNS_JAVA:%=src/javax/jmdns/%)

POPDIRS = hopdroid androidemo

#*---------------------------------------------------------------------*/
#*    build                                                            */
#*---------------------------------------------------------------------*/
build: /tmp/droidcc.sh
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

#*---------------------------------------------------------------------*/
#*    Common files                                                     */
#*---------------------------------------------------------------------*/
$(ANDROIDBUILDDIR):
	mkdir -p $(ANDROIDBUILDDIR)

$(ANDROIDBUILDDIR)/AndroidManifest.xml $(ANDROIDBUILDDIR)/project.properties $(ANDROIDBUILDDIR)/Android.mk:
	cat $< \
          | sed -e "s|@HOPVERSION@|$(HOPRELEASE)|" \
                -e "s|@HOPDEBUG@|$(ANDROIDHOPDEBUG)|" \
                -e "s|@ANDROIDSDKVERSION@|$(ANDROIDSDKVERSION)|" \
                -e "s|@ANDROIDSDK@|$(ANDROIDSDK)|" \
                -e "s|@BASEDIR@|$(BUILDDIR)/arch/android|" \
                -e "s|@HOPPORT@|$(ANDROIDHOPPORT)|" \
                -e "s|@HOPTHREADS@|$(ANDROIDHOPTHREADS)|" \
                -e "s|@HOPZEROCONF@|$(ANDROIDHOPZEROCONF)|" \
                -e "s|@HOPAPKNAME@|$(HOPAPKNAME)|" \
                -e "s|@HOPAPP@|$(HOPAPP)|" \
                -e "s|@HOPAPPVERSION@|$(HOPAPPVERSION)|" \
                -e "s|@HOPURL@|$(ANDROIDHOPURL)|" > $@ || exit 1

$(ANDROIDBUILDDIR)/AndroidManifest.xml: $(ANDROIDMANIFEST).in $(ANDROIDBUILDDIR)
$(ANDROIDBUILDDIR)/project.properties: $(ANDROIDDIR)/project.properties.in $(ANDROIDBUILDDIR)
$(ANDROIDBUILDDIR)/Android.mk: $(ANDROIDDIR)/Android.mk.in $(ANDROIDBUILDDIR)

#*---------------------------------------------------------------------*/
#*    res                                                              */
#*---------------------------------------------------------------------*/
$(ANDROIDBUILDDIR)/res: $(ANDROIDRES) $(ANDROIDBUILDDIR)
	$(RM) -rf $@
	cp -r $(ANDROIDRES) $@

$(ANDROIDBUILDDIR)/res/values/strings.xml \
  $(ANDROIDBUILDDIR)/res/values/plugins.xml \
  $(ANDROIDBUILDDIR)/res/values/config.xml \
  $(ANDROIDBUILDDIR)/res/xml/preferences.xml \
  $(ANDROIDBUILDDIR)/src/fr/inria/hop/Hop.java:
	rm -f $@.in
	hz=`basename $(HOPHZ)` && cat $< \
          | sed -e "s|@HOPVERSION@|$(HOPRELEASE)|" \
		-e "s|@HOPAPK@|$(HOPAPK)|" \
		-e "s|@HOPHZ@|$$hz|" \
		-e "s|@BIGLOOVERSION@|$(RELEASE)|" \
                -e "s|@HOPPORT@|$(ANDROIDHOPPORT)|" \
                -e "s|@HOPTHREADS@|$(ANDROIDHOPTHREADS)|" \
                -e "s|@HOPCONSOLELOG@|$(ANDROIDHOPCONSOLELOG)|" \
                -e "s|@HOPDEBUGLEVEL@|$(ANDROIDHOPDEBUGLEVEL)|" \
                -e "s|@HOPVERBOSELEVEL@|$(ANDROIDHOPVERBOSELEVEL)|" \
                -e "s|@HOPZEROCONF@|$(ANDROIDHOPZEROCONF)|" \
                -e "s|@HOPPREFIX@|$(ANDROIDHOPPREFIX)|" \
                -e "s|@HOPAPP@|$(HOPAPP)|" \
                -e "s|@HOPARGS@|$(HOPARGS)|" \
                -e "s|@HOPAPPVERSION@|$(HOPAPPVERSION)|" \
                -e "s|@HOPURL@|$(ANDROIDHOPURL)|" \
                -e "s|@NOTITLE@|$(NOTITLE)|" \
                -e "s|@DEFLANG@|$(DEFLANG)|" \
                -e "s|@CUSTOMTITLE@|$(CUSTOMTITLE)|" \
                -e "s|@STATUSBARCOLOR@|$(STATUSBARCOLOR)|" \
                -e "s|@PLUGINBUILD@|$(PLUGINBUILD)|" \
                -e "s|@PLUGINLOCALE@|$(PLUGINLOCALE)|" \
                -e "s|@PLUGINVIBRATE@|$(PLUGINVIBRATE)|" \
                -e "s|@PLUGINMUSICPLAYER@|$(PLUGINMUSICPLAYER)|" \
                -e "s|@PLUGINMEDIAAUDIO@|$(PLUGINMEDIAAUDIO)|" \
                -e "s|@PLUGINSENSOR@|$(PLUGINSENSOR)|" \
                -e "s|@PLUGINBATTERY@|$(PLUGINBATTERY)|" \
                -e "s|@PLUGINSMS@|$(PLUGINSMS)|" \
                -e "s|@PLUGINWIFI@|$(PLUGINWIFI)|" \
                -e "s|@PLUGINCONNECTIVITY@|$(PLUGINCONNECTIVITY)|" \
                -e "s|@PLUGINCONTACT@|$(PLUGINCONTACT)|" \
                -e "s|@PLUGINZEROCONF@|$(PLUGINZEROCONF)|" \
                -e "s|@PLUGINSYSTEM@|$(PLUGINSYSTEM)|" \
                -e "s|@PLUGINTTS@|$(PLUGINTTS)|" \
                -e "s|@PLUGINPREFS@|$(PLUGINPREFS)|" \
                -e "s|@PLUGINCALL@|$(PLUGINCALL)|" > $@

$(ANDROIDBUILDDIR)/res/values/strings.xml: \
   $(ANDROIDDIR)/res/values/strings.xml.in \
   $(ANDROIDBUILDDIR)/res
$(ANDROIDBUILDDIR)/res/values/plugins.xml: \
   $(ANDROIDDIR)/res/values/plugins.xml.in \
   $(ANDROIDBUILDDIR)/res
$(ANDROIDBUILDDIR)/res/values/config.xml: \
   $(ANDROIDDIR)/res/values/config.xml.in \
   $(ANDROIDBUILDDIR)/res
$(ANDROIDBUILDDIR)/res/xml/preferences.xml: \
   $(ANDROIDDIR)/res/xml/preferences.xml.in \
   $(ANDROIDBUILDDIR)/res

#*---------------------------------------------------------------------*/
#*    assets                                                           */
#*---------------------------------------------------------------------*/
.PHONY: $(ANDROIDBUILDDIR)/assets $(ANDROIDBUILDDIR)/assets.client

$(ANDROIDBUILDDIR)/assets: \
  $(ANDROIDBUILDDIR)/assets/etc \
  $(ANDROIDBUILDDIR)/assets/bin \
  $(ANDROIDBUILDDIR)/assets/lib \
  $(ANDROIDBUILDDIR)/assets/share \
  $(ANDROIDBUILDDIR)/assets/cache \
  $(ANDROIDBUILDDIR)/assets/rcdir \
  $(ANDROIDBUILDDIR)/assets/splash
	$(MAKE) rename-afiles
	$(MAKE) assets.weblet
	$(MAKE) assets.module
	$(MAKE) assets.post-install

$(ANDROIDBUILDDIR)/assets.client: \
  $(ANDROIDBUILDDIR)/assets/etc \
  $(ANDROIDBUILDDIR)/assets/cache \
  $(ANDROIDBUILDDIR)/assets/rcdir \
  $(ANDROIDBUILDDIR)/assets/hz \
  $(ANDROIDBUILDDIR)/assets/splash
	$(MAKE) rename-afiles
	$(MAKE) assets.post-install.client

assets.weblet:
	if [ "$(ANDROIDWEBLET) " != " " ]; then \
          cp -r $(ANDROIDWEBLET) $(ANDROIDBUILDDIR)/assets/lib/hop/$(HOPRELEASE)/weblets; \
	  make -C $(ANDROIDBUILDDIR)/assets/lib/hop/$(HOPRELEASE)/weblets/`basename $(ANDROIDWEBLET)`/arch/android -f Makefile.android prepare-android-weblet; \
        fi

assets.module:
	if [ "$(ANDROIDMODULE) " != " " ]; then \
          cp -r $(ANDROIDMODULE) $(ANDROIDBUILDDIR)/assets/lib/hop/$(HOPRELEASE)/node_modules; \
	  make -C $(ANDROIDBUILDDIR)/assets/lib/hop/$(HOPRELEASE)/node_modules/`basename $(ANDROIDMODULE)`/arch/android -f Makefile.android prepare-android-module; \
        fi

assets.post-install: assets.post-install.$(ANDROIDLINK)
	find $(ANDROIDBUILDDIR)/assets/lib/ -name '*.heap' -exec rm {} \;
	find $(ANDROIDBUILDDIR)/assets/lib/ -name '*.a' -exec rm {} \;
	for w in $(ANDROID_UNUSED_WEBLETS); do \
          rm -rf $(ANDROIDBUILDDIR)/assets/lib/hop/$(HOPRELEASE)/weblets/$$w; \
        done

assets.post-install.dynamic:
	find $(ANDROIDBUILDDIR)/assets/lib/ -name 'libhop*_u-*.so' -exec rm {} \;
	find $(ANDROIDBUILDDIR)/assets/lib/ -name 'libjs2scheme*_u-*.so' -exec rm {} \;
	find $(ANDROIDBUILDDIR)/assets/lib/ -name 'libnodejs*_u-*.so' -exec rm {} \;
	find $(ANDROIDBUILDDIR)/assets/lib/ -name 'libscheme2js*_u-*.so' -exec rm {} \;
	find $(ANDROIDBUILDDIR)/assets/lib/ -name 'libbigloo*_u-*.so.keep' -exec rm {} \;

assets.post-install.static:
	find $(ANDROIDBUILDDIR)/assets/lib/ -name 'libhop*_[su]-*.so' -exec rm {} \;
	find $(ANDROIDBUILDDIR)/assets/lib/ -name 'libjs2scheme*_[su]-*.so' -exec rm {} \;
	find $(ANDROIDBUILDDIR)/assets/lib/ -name 'libnodejs*_[su]-*.so' -exec rm {} \;
	find $(ANDROIDBUILDDIR)/assets/lib/ -name 'libscheme2js*_[su]-*.so' -exec rm {} \;
	find $(ANDROIDBUILDDIR)/assets/lib/ -name 'libbigloo*_[su]-*.so.keep' -exec rm {} \;

assets.post-install.client:

.PHONY: $(ANDROIDBUILDDIR)/assets/etc
$(ANDROIDBUILDDIR)/assets/etc: splash/splash.html splash/hopnotfound.html splash/hoperror.html
	mkdir -p $(ANDROIDBUILDDIR)/assets
	rm -rf $@
	cp -r $(ANDROIDBUILDDIR)$(ANDROIDHOPPREFIX)/etc $(ANDROIDBUILDDIR)/assets
	# mkdir -p $(ANDROIDBUILDDIR)/assets/etc/hop/$(HOPRELEASE)


$(ANDROIDBUILDDIR)/assets/splash:
	mkdir -p $(ANDROIDBUILDDIR)/assets
	rm -rf $@
	mkdir -p $@
	$(MAKE) $(ANDROIDBUILDDIR)/assets/splash/splash.html
	$(MAKE) $(ANDROIDBUILDDIR)/assets/splash/hopnotfound.html
	$(MAKE) $(ANDROIDBUILDDIR)/assets/splash/hoperror.html

.PHONY: $(ANDROIDBUILDDIR)/assets/share
$(ANDROIDBUILDDIR)/assets/share:
	mkdir -p $(ANDROIDBUILDDIR)/assets
	rm -rf $@
	cp -r $(ANDROIDBUILDDIR)$(ANDROIDHOPPREFIX)/share $(ANDROIDBUILDDIR)/assets
	$(MAKE) rename-jsgz

$(ANDROIDBUILDDIR)/assets/cache:
	mkdir -p $(ANDROIDBUILDDIR)/assets/cache

$(ANDROIDBUILDDIR)/assets/rcdir: rcdir
	mkdir -p $(ANDROIDBUILDDIR)/assets
	rm -rf $@
	cp -r $(ANDROIDDIR)/rcdir $(ANDROIDBUILDDIR)/assets

$(ANDROIDBUILDDIR)/assets/hz: $(HOPHZ)
	if [ "$(HOPHZ) " != " " ]; then \
	  mkdir -p $(ANDROIDBUILDDIR)/assets/hz; \
	  cp $(HOPHZ) $(ANDROIDBUILDDIR)/assets/hz; \
        fi

rename-jsgz:
	for jsgz in `find $(ANDROIDBUILDDIR)/assets/share/hop -name '*.js.gz' -print`; do \
          base=`basename $$jsgz .js.gz`; \
          if [ "$$base " !=  "* " ]; then \
            mv $$jsgz `dirname $$jsgz`/$$base.jsgz; \
          fi \
        done

$(ANDROIDBUILDDIR)/assets/bin: $(ANDROIDBUILDDIR)/assets/bin/hop

$(ANDROIDBUILDDIR)/assets/bin/hop:
	mkdir -p $(ANDROIDBUILDDIR)/assets/bin
	cp $(ANDROIDBUILDDIR)$(ANDROIDHOPPREFIX)/bin/hop $@

$(ANDROIDBUILDDIR)/assets/lib: \
  $(ANDROIDBUILDDIR)/assets/lib/bigloo \
  $(ANDROIDBUILDDIR)/assets/lib/hop

.PHONY: $(ANDROIDBUILDDIR)/assets/lib/bigloo
$(ANDROIDBUILDDIR)/assets/lib/bigloo:
	mkdir -p $(ANDROIDBUILDDIR)/assets/lib
	rm -rf $@
	cp -r $(ANDROIDBUILDDIR)$(ANDROIDHOPPREFIX)/lib/bigloo $(ANDROIDBUILDDIR)/assets/lib

.PHONY: $(ANDROIDBUILDDIR)/assets/lib/hop
$(ANDROIDBUILDDIR)/assets/lib/hop:
	mkdir -p $(ANDROIDBUILDDIR)/assets/lib
	rm -rf $@
	cp -r $(ANDROIDBUILDDIR)$(ANDROIDHOPPREFIX)/lib/hop $(ANDROIDBUILDDIR)/assets/lib

rename-afiles:
	for afile in `find $(ANDROIDBUILDDIR)/assets -name .afile -print`; do \
          dot_afile=`dirname $$afile`/dot`basename $$afile`; \
          mv $$afile $$dot_afile; \
        done

ndk-build: $(ANDROIDBUILDDIR)/assets
	$(ANDROIDNDK)/ndk-build -C $(ANDROIDBUILDDIR) V=1

ndk-build.client: $(ANDROIDBUILDDIR)/assets.client
	$(ANDROIDNDK)/ndk-build -C $(ANDROIDBUILDDIR) V=1

apk-build:
	(hopdir=`pwd`; cd $(ANDROIDBUILDDIR); \
           $$hopdir/buildapk.sh \
           --androidsdk=$(ANDROIDSDK) \
           --androidndk=$(ANDROIDNDK) \
           --javac=$(JAVAC) \
           --androidkeystore=$(ANDROIDKEYSTORE) \
           $(HOPAPKNAME))

apk-build.client: 
	(hopdir=`pwd`; cd $(ANDROIDBUILDDIR); \
           $$hopdir/buildapk.sh \
           --androidsdk=$(ANDROIDSDK) \
           --androidndk=$(ANDROIDNDK) \
           --javac=$(JAVAC) \
           --androidkeystore=$(ANDROIDKEYSTORE) \
           --pluginjava="$(PLUGIN_JAVA)" \
           --hopapp=$(HOPAPP) \
           --nolibdir \
           $(HOPAPKNAME))

#*---------------------------------------------------------------------*/
#*    apk                                                              */
#*---------------------------------------------------------------------*/
.PHONY: apk install.apk click.apk uninstall.apk \
  apk.bigloo apk.bigloo.post-install apk.hop apk.lib

apk: apk.bigloo apk.bigloo.post-install
	$(MAKE) apk-sans-bigloo

apk-sans-bigloo: apk.common apk.res apk.jni apk.src apk.hop apk.custom
	$(MAKE) ndk-build
	$(MAKE) apk-build

apk-sans-hop: apk.common apk.res apk.jni apk.src apk.weblets
	$(MAKE) ndk-build
	$(MAKE) apk-build

apk-re-hop: apk.common apk.res apk.jni apk.src apk.rehop apk.recustom
	$(MAKE) ndk-build
	$(MAKE) apk-build

apk-sans-bigloo.client: apk.common apk.res apk.jni apk.src apk.hop.client
	$(MAKE) ndk-build.client
	$(MAKE) apk-build.client

apk-sans-hop.client: apk.common apk.res apk.jni apk.src.client apk.plugin.client apk.hop.client
	$(MAKE) ndk-build.client
	$(MAKE) apk-build.client

install.apk:
	adb $(ANDROIDHOST) install $(ANDROIDBUILDDIR)/bin/$(HOPAPKNAME).apk

click.apk:
	adb $(ANDROIDHOST) shell monkey -p fr.inria.hop 1

uninstall.apk:
	adb $(ANDROIDHOST) uninstall fr.inria.hop

apk.common: $(ANDROIDBUILDDIR)/Android.mk \
  $(ANDROIDBUILDDIR)/AndroidManifest.xml \
  $(ANDROIDBUILDDIR)/project.properties

apk.res: $(ANDROIDBUILDDIR)/res \
  $(ANDROIDBUILDDIR)/res/values/strings.xml \
  $(ANDROIDBUILDDIR)/res/values/plugins.xml \
  $(ANDROIDBUILDDIR)/res/values/config.xml \
  $(ANDROIDBUILDDIR)/res/xml/preferences.xml

apk.jni: $(ANDROIDBUILDDIR)/jni
$(ANDROIDBUILDDIR)/jni: jni $(ANDROIDBUILDDIR)
	cp -r $< $@
	for p in `find $(ANDROIDBUILDDIR)/jni -name '*.cpp' -print`; do \
           cat $$p | sed 's|fr/inria/hop/|fr/inria/$(HOPAPP)/|g' > $$p.__tmp; \
           mv $$p.__tmp $$p; \
        done
	for p in `find $(ANDROIDBUILDDIR)/jni -name '*.c' -print`; do \
           cat $$p | sed 's|fr/inria/hop/|fr/inria/$(HOPAPP)/|g' > $$p.__tmp; \
           mv $$p.__tmp $$p; \
        done

# this goal is only used by the apk-sans-hop target
apk.weblets:
	$(RM) -rf $(ANDROIDBUILDDIR)/assets/lib/hop/$(HOPRELEASE)/weblets
	$(RM) -rf $(ANDROIDBUILDDIR)/$(HOPFULLNAME)/weblets
	cp -r $(BUILDDIR)/weblets $(ANDROIDBUILDDIR)/$(HOPFULLNAME)
	cp -r $(ANDROIDBUILDDIR)/$(HOPFULLNAME)/weblets $(ANDROIDBUILDDIR)/assets/lib/hop/$(HOPRELEASE)

apk.src: $(ANDROIDBUILDDIR)/src
$(ANDROIDBUILDDIR)/src: $(ANDROIDDIR)/src $(ANDROIDBUILDDIR) \
  $(ANDROID_JAVA:%=$(ANDROIDDIR)/src/fr/inria/hop/%)
	$(RM) -rf $@
	cp -r $(ANDROIDDIR)/src $@
	for p in `find $(ANDROIDBUILDDIR)/src -name '*.java' -print`; do \
           cat $$p \
               | sed -e 's|fr.inria.hop;|fr.inria.$(HOPAPP);|g' \
                     -e 's|fr.inria.hop[.]|fr.inria.$(HOPAPP).|g' \
               > $$p.__tmp; \
           mv $$p.__tmp $$p; \
        done
	if [ "$(HOPAPP)" != "hop" ]; then \
	  mv $(ANDROIDBUILDDIR)/src/fr/inria/hop $(ANDROIDBUILDDIR)/src/fr/inria/$(HOPAPP); \
        fi

apk.src.client: apk.src
	if [ -d $(ANDROIDSRCCLIENT) ]; then \
	   cp -r $(ANDROIDSRCCLIENT) $(ANDROIDBUILDDIR); \
	fi

apk.plugin.client:
	if [ "$(PLUGIN_JAVA) " != " " ]l; then \
	   $(RM) -rf $(ANDROIDBUILDDIR)/tmpsrc && \
	   $(RM) -rf $(ANDROIDBUILDDIR)/plugin && \
	   $(MAKE) apk.src.client ANDROIDBUILDDIR=$(ANDROIDBUILDDIR)/tmpsrc HOPAPP=hop && \
	   mv $(ANDROIDBUILDDIR)/tmpsrc/src $(ANDROIDBUILDDIR)/plugin && \
	   rm -rf $(ANDROIDBUILDDIR)/tmpsrc || exit 1; \
        fi
	for p in $(PLUGIN_JAVA); do \
	   cp $(ANDROIDWEBLET)/arch/android/$$p \
              $(ANDROIDBUILDDIR)/plugin/fr/inria/hop/`basename $$p`; \
        done

apk.bigloo: $(ANDROIDBUILDDIR)/bigloo-$(RELEASE) /tmp/droidcc.sh
	$(MAKE) apk.bigloo.configure
	$(MAKE) -C $(ANDROIDBUILDDIR)/bigloo-$(RELEASE)
	$(MAKE) -C $(ANDROIDBUILDDIR)/bigloo-$(RELEASE) install DESTDIR=$(ANDROIDBUILDDIR)

apk.bigloo.post-install: apk.bigloo
	rm $(ANDROIDBUILDDIR)/$(ANDROIDHOPPREFIX)/lib/bigloo/$(RELEASE)/libpcre.so.1
	cp $(ANDROIDBUILDDIR)/$(ANDROIDHOPPREFIX)/lib/bigloo/$(RELEASE)/libpcre.so.1.2.11 \
           $(ANDROIDBUILDDIR)/$(ANDROIDHOPPREFIX)/lib/bigloo/$(RELEASE)/libpcre.so.1

apk.bigloo.configure:
	(cd $(ANDROIDBUILDDIR)/bigloo-$(RELEASE); \
         ./configure --os-android \
            --android-adb=adb \
            --cc=/tmp/droidcc.sh \
            --cpicflags=-fPIC \
            --cflags="-fPIC -DBGL_GC_ROOTS" \
            --lflags="-fPIC" \
            --customlibuv=yes \
            --customlibuvversion=v1.20.3 \
            --libuvconfigureopt="--host=arm-linux-androideabi" \
            --custompcre=yes \
            --stack-check=no \
            --prefix=$(ANDROIDHOPPREFIX) \
            --build-bindir=$(HOPBINDIR) \
            --disable-doc --disable-unistring --disable-gmp \
            $(BIGLOOCONFIGUREOPT))

$(ANDROIDBUILDDIR)/bigloo-$(RELEASE): $(REPOSITORY)/bigloo-$(RELEASE).tar.gz
	mkdir -p $(ANDROIDBUILDDIR)
	(cd $(ANDROIDBUILDDIR); rm -rf $(ANDROIDBUILDDIR)/bigloo-$(RELEASE))
	(cd $(ANDROIDBUILDDIR); tar xf $< 2> /dev/null > /dev/null)

apk.hop: $(ANDROIDBUILDDIR)/$(HOPFULLNAME)
	$(MAKE) apk.hop.configure
	$(MAKE) -C $(ANDROIDBUILDDIR)/$(HOPFULLNAME)
	$(MAKE) -C $(ANDROIDBUILDDIR)/$(HOPFULLNAME) install DESTDIR=$(ANDROIDBUILDDIR)

apk.rehop:
	$(MAKE) -C $(ANDROIDBUILDDIR)/$(HOPFULLNAME)
	$(MAKE) -C $(ANDROIDBUILDDIR)/$(HOPFULLNAME) install DESTDIR=$(ANDROIDBUILDDIR)

apk.hop.client: $(ANDROIDBUILDDIR)/$(HOPFULLNAME)
	mkdir -p $(ANDROIDBUILDDIR)$(ANDROIDHOPPREFIX)/etc
	cp -r $(ANDROIDBUILDDIR)/$(HOPFULLNAME)/etc/hoprc.hop $(ANDROIDBUILDDIR)$(ANDROIDHOPPREFIX)/etc

apk.hop.configure:
	(cd $(ANDROIDBUILDDIR)/$(HOPFULLNAME); \
         ./configure \
            --bigloo=$(HOPBINDIR)/bigloo \
            --bigloolibdir=$(ANDROIDBUILDDIR)$(ANDROIDHOPPREFIX)/lib/bigloo/$(RELEASE) \
            --android \
            --android-app=$(HOPAPP) \
            --link=$(ANDROIDLINK) \
            --hopc=$(HOPBINDIR)/hopc \
            --hop=$(HOPBINDIR)/hop \
            --prefix=$(ANDROIDHOPPREFIX) \
            --libdir=$(ANDROIDHOPPREFIX)/lib \
            --library=mail \
            --library=calendar \
            --library=text \
            --library=phone \
            --disable-doc \
            --bigloo-pcre=no --bigloo-unistring=no \
            $(HOPCONFIGUREOPT))

$(ANDROIDBUILDDIR)/$(HOPFULLNAME):
	if [ -d $(HOPSOURCE) ]; then \
	  (cd $(ANDROIDBUILDDIR); rm -rf $(ANDROIDBUILDDIR)/hop $(ANDROIDBUILDDIR)/$(HOPFULLNAME)) && \
          (cd $(ANDROIDBUILDDIR); git clone $(HOPSOURCE)) && \
          (cd $(ANDROIDBUILDDIR); cp $(HOPSOURCE)/share/hop-builtin-serializers.js $(ANDROIDBUILDDIR)/hop/share) && \
          (cd $(ANDROIDBUILDDIR); cp $(HOPSOURCE)/etc/hoprc.hop $(ANDROIDBUILDDIR)/hop/etc) && \
          (mv $(ANDROIDBUILDDIR)/hop $(ANDROIDBUILDDIR)/$(HOPFULLNAME)) || exit 1; \
        else \
	  (cd $(ANDROIDBUILDDIR); rm -rf $(ANDROIDBUILDDIR)/$(HOPFULLNAME)) && \
	  (cd $(ANDROIDBUILDDIR); tar xf $(HOPSOURCE)) || exit 1; \
        fi

apk-clean:
	rm -rf $(ANDROIDBUILDDIR)
	rm -f $(HOPTMPDIR)/antandroid.sh

apk.custom: apk.hop $(ANDROIDBUILDDIR)/assets
	if [ "$(APKCUSTOM) " != " " ]; then \
	   $(MAKE) -C $(APKCUSTOMDIR) -f Makefile.android $(APKCUSTOM) \
               HOPBINDIR=$(HOPBINDIR) \
               HOPRELEASE=$(HOPRELEASE) \
               RELEASE=$(RELEASE) \
               ANDROIDBUILDDIR=$(ANDROIDBUILDDIR) \
               ANDROIDHOPPREFIX=$(ANDROIDHOPPREFIX); \
	fi

apk.recustom:
	rm -rf $(ANDROIDBUILDDIR)/assets
	$(MAKE) apk.custom

#*---------------------------------------------------------------------*/
#*    droidcc ...                                                      */
#*---------------------------------------------------------------------*/
/tmp/droidcc.sh: Makefile
	@echo "#!/bin/bash" > $@
	@echo "exec $(ANDROIDCC) -target $(ANDROIDTARGET) \"\$$@\"" >> $@
	@chmod a+rx /tmp/droidcc.sh

#*---------------------------------------------------------------------*/
#*    splash                                                           */
#*---------------------------------------------------------------------*/
splash/splash.html: splash/splash.js splash/hop.svgz
	hop --no-server $< > $@

splash/hopnotfound.html: splash/hopnotfound.js splash/hop.svgz
	hop --no-server $< > $@

splash/hoperror.html: splash/hoperror.js splash/hop.svgz
	hop --no-server $< > $@

$(ANDROIDBUILDDIR)/assets/splash/%.html: $(ANDROIDSPLASH)/%.html
	cp $< $@

$(ANDROIDSPLASH)/%.html: $(ANDROIDSPLASH)/%.js
	hop --no-server $< > $@

