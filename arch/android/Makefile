#*=====================================================================*/
#*    serrano/prgm/project/hop/3.1.x/arch/android/Makefile             */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Tue Oct 12 11:53:03 2010                          */
#*    Last change :  Wed Mar  1 16:41:12 2017 (serrano)                */
#*    Copyright   :  2010-17 Manuel Serrano                            */
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
#*    Configuration                                                    */
#*---------------------------------------------------------------------*/
ANDROIDBUILDDIR = $(HOPTMPDIR)/build.hop.android
ANDROIDROOT = /opt/android
ANDROIDSDK = $(ANDROIDROOT)/android-sdk-linux
ANDROIDNDK = $(ANDROIDROOT)/android-ndk-r9d
ANDROIDTOOLCHAIN = $(ANDROIDROOT)/toolchain
ANDROIDSDKVERSION = 21
ANDROIDACTIONMODE = debug
ANDROIDHOST = -d
ANDROIDDIR= $(BUILDDIR)/arch/android

ANDROIDANT = ant

# hop androin configuration
ANDROIDHOPPORT = 8080
ANDROIDHOPTHREADS = 6
ANDROIDHOPZEROCONF = true
ANDROIDHOPDEBUG = true
ANDROIDHOPURL = http://hop.inria.fr
ANDROIDHOPCONSOLELOG = false
ANDROIDHOPDEBUGLEVEL = -g0
ANDROIDHOPVERBOSELEVEL = -v
ANDROIDHOPPREFIX = /data/data/fr.inria.hop/assets

ANDROIDWEBLET = 

# Plugins
PLUGINBUILD = true
PLUGINLOCALE = false
PLUGINVIBRATE = false
PLUGINMUSICPLAYER = true
PLUGINMEDIAAUDIO = true
PLUGINSENSOR = false
PLUGINBATTERY = false
PLUGINSMS = false
PLUGINWIFI = false
PLUGINCONNECTIVITY = false
PLUGINCONTACT = false
PLUGINZEROCONF = true
PLUGINSYSTEM = false
PLUGINTTS = false
PLUGINCALL = false
PLUGINPREFS = false

# global configuration
REPOSITORY = $(DISTRIBDIR)

HOPAPKNAME = hop
HOPAPP = hopdac
HOPARGS = 
HOPMINOR = -$(HOPDEVEL)1
HOPFULLNAME = hop-$(HOPRELEASE)$(HOPMINOR)

#*---------------------------------------------------------------------*/
#*    Sources                                                          */
#*---------------------------------------------------------------------*/
ANDROID_BUILD = Android.mk.in build.xml.in project.properties.in hop.keystore \
  AndroidManifest.xml.in local.properties.in ant.properties.in proguard.cfg \
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
  res/values/plugins.xml.in res/menu/hop_menu.xml

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
  HopConfig.java

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
POPULATION = Makefile makeapk.sh.in \
  $(ANDROID_BUILD) $(ANDROID_JNI) $(ANDROID_RES) \
  $(ANDROID_JAVA:%=src/fr/inria/hop/%) \
  $(ANDROID_JMDNS_JAVA:%=src/javax/jmdns/%)

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

#*---------------------------------------------------------------------*/
#*    Common files                                                     */
#*---------------------------------------------------------------------*/
$(ANDROIDBUILDDIR):
	mkdir -p $(ANDROIDBUILDDIR)

$(ANDROIDBUILDDIR)/AndroidManifest.xml $(ANDROIDBUILDDIR)/project.properties $(ANDROIDBUILDDIR)/build.xml $(ANDROIDBUILDDIR)/Android.mk $(ANDROIDBUILDDIR)/local.properties $(ANDROIDBUILDDIR)/ant.properties:
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
                -e "s|@HOPURL@|$(ANDROIDHOPURL)|" > $@ || exit 1

$(ANDROIDBUILDDIR)/AndroidManifest.xml: $(ANDROIDDIR)/AndroidManifest.xml.in $(ANDROIDBUILDDIR)
$(ANDROIDBUILDDIR)/project.properties: $(ANDROIDDIR)/project.properties.in $(ANDROIDBUILDDIR)
$(ANDROIDBUILDDIR)/build.xml: $(ANDROIDDIR)/build.xml.in $(ANDROIDBUILDDIR)
$(ANDROIDBUILDDIR)/Android.mk: $(ANDROIDDIR)/Android.mk.in $(ANDROIDBUILDDIR)
$(ANDROIDBUILDDIR)/local.properties: $(ANDROIDDIR)/local.properties.in $(ANDROIDBUILDDIR)
$(ANDROIDBUILDDIR)/ant.properties: $(ANDROIDDIR)/ant.properties.in $(ANDROIDBUILDDIR)

#*---------------------------------------------------------------------*/
#*    res                                                              */
#*---------------------------------------------------------------------*/
$(ANDROIDBUILDDIR)/res: res $(ANDROIDBUILDDIR)
	$(RM) -rf $@
	cp -r $(ANDROIDDIR)/res $@

$(ANDROIDBUILDDIR)/res/values/strings.xml $(ANDROIDBUILDDIR)/res/values/plugins.xml $(ANDROIDBUILDDIR)/res/xml/preferences.xml $(ANDROIDBUILDDIR)/src/fr/inria/hop/Hop.java:
	rm -f $@.in
	cat $< \
          | sed -e "s|@HOPVERSION@|$(HOPRELEASE)|" \
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
                -e "s|@HOPURL@|$(ANDROIDHOPURL)|" \
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
$(ANDROIDBUILDDIR)/res/xml/preferences.xml: \
   $(ANDROIDDIR)/res/xml/preferences.xml.in \
   $(ANDROIDBUILDDIR)/res

#*---------------------------------------------------------------------*/
#*    assets                                                           */
#*---------------------------------------------------------------------*/
$(ANDROIDBUILDDIR)/assets: \
  $(ANDROIDBUILDDIR)/assets/etc \
  $(ANDROIDBUILDDIR)/assets/bin \
  $(ANDROIDBUILDDIR)/assets/lib \
  $(ANDROIDBUILDDIR)/assets/share
	$(MAKE) rename-afiles
	$(MAKE) assets.weblet
	$(MAKE) assets.post-install

assets.weblet:
	if [ "$(ANDROIDWEBLET) " != " " ]; then \
          cp -r $(ANDROIDWEBLET) $(ANDROIDBUILDDIR)/assets/lib/hop/$(HOPRELEASE)/weblets; \
	  make -C $(ANDROIDBUILDDIR)/assets/lib/hop/$(HOPRELEASE)/weblets/`basename $(ANDROIDWEBLET)` -f $(ANDROIDBUILDDIR)/assets/lib/hop/$(HOPRELEASE)/weblets/`basename $(ANDROIDWEBLET)`/arch/android/Makefile.hopdac prepare-android-weblet; \
        fi

assets.post-install:
	find $(ANDROIDBUILDDIR)/assets/lib/ -name '*.a' -exec rm {} \;
	find $(ANDROIDBUILDDIR)/assets/lib/ -name '*.heap' -exec rm {} \;
	find $(ANDROIDBUILDDIR)/assets/lib/ -name 'libhop*_u-*.so' -exec rm {} \;
	find $(ANDROIDBUILDDIR)/assets/lib/ -name 'libjs2scheme*_u-*.so' -exec rm {} \;
	find $(ANDROIDBUILDDIR)/assets/lib/ -name 'libnodejs*_u-*.so' -exec rm {} \;
	find $(ANDROIDBUILDDIR)/assets/lib/ -name 'libscheme2js*_u-*.so' -exec rm {} \;
	find $(ANDROIDBUILDDIR)/assets/lib/ -name 'libbigloo*_s-*.so.keep' -exec rm {} \;

$(ANDROIDBUILDDIR)/assets/etc:
	mkdir -p $(ANDROIDBUILDDIR)/assets
	rm -rf $@
	cp -r $(ANDROIDBUILDDIR)$(ANDROIDHOPPREFIX)/etc $(ANDROIDBUILDDIR)/assets
	mkdir -p $(ANDROIDBUILDDIR)/assets/etc/hop/$(HOPRELEASE)
	cp -r $(ANDROIDDIR)/splash $(ANDROIDBUILDDIR)/assets/etc/hop/$(HOPRELEASE)

$(ANDROIDBUILDDIR)/assets/share:
	mkdir -p $(ANDROIDBUILDDIR)/assets
	rm -rf $@
	cp -r $(ANDROIDBUILDDIR)$(ANDROIDHOPPREFIX)/share $(ANDROIDBUILDDIR)/assets
	$(MAKE) rename-jsgz

rename-jsgz:
	for jsgz in `find $(ANDROIDBUILDDIR)/assets/share/hop -name '*.js.gz' -print`; do \
          base=`basename $$jsgz .js.gz`; \
          if [ "$$base " !=  "* " ]; then \
            mv $$jsgz `dirname $$jsgz`/$$base.jsgz; \
          fi \
        done

$(ANDROIDBUILDDIR)/assets/bin: $(ANDROIDBUILDDIR)/assets/bin
	mkdir -p $@
	cp $(ANDROIDBUILDDIR)$(ANDROIDHOPPREFIX)/bin/hop $@

$(ANDROIDBUILDDIR)/assets/lib: \
  $(ANDROIDBUILDDIR)/assets/lib/bigloo \
  $(ANDROIDBUILDDIR)/assets/lib/hop

$(ANDROIDBUILDDIR)/assets/lib/bigloo:
	mkdir -p $(ANDROIDBUILDDIR)/assets/lib
	rm -rf $@
	cp -r $(ANDROIDBUILDDIR)$(ANDROIDHOPPREFIX)/lib/bigloo $(ANDROIDBUILDDIR)/assets/lib

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
	$(ANDROIDNDK)/ndk-build -C $(ANDROIDBUILDDIR) V=1 || exit 1

ndk-ant: $(HOPTMPDIR)/antandroid.sh $(ANDROIDBUILDDIR)/src/fr/inria/hop/Hop.java
	(cd $(ANDROIDBUILDDIR) && $(HOPTMPDIR)/antandroid.sh) || exit 1

#*---------------------------------------------------------------------*/
#*    HOPTMPDIR/antandroid.sh ...                                      */
#*---------------------------------------------------------------------*/
$(HOPTMPDIR)/antandroid.sh:
	echo "#!/bin/sh" > $@
	echo "$(ANDROIDANT) -Dbuild.compiler.fulldepend=true -Dbuild.compiler.compilerarg="-Xlint:unchecked" $(ANDROIDACTIONMODE) <<EOF" >> $@
	echo "hophop" >> $@
	echo "hophop" >> $@
	echo "" >> $@
	echo "EOF" >> $@
	echo "" >> $@
	echo "if [ \"$$""? \" != \"0 \" ]; then" >> $@
	echo "  exit 1" >> $@
	echo "fi" >> $@
	chmod a+rx $@

#*---------------------------------------------------------------------*/
#*    apk                                                              */
#*---------------------------------------------------------------------*/
.PHONY: apk install.apk click.apk apk.bigloo apk.hop apk.hop.android apk.lib

apk: apk.bigloo
	$(MAKE) apk-sans-bigloo

apk-sans-bigloo: apk.common apk.res apk.jni apk.src apk.hop
	$(MAKE) ndk-build
	$(MAKE) ndk-ant

apk-sans-hop: apk.common apk.res apk.jni apk.src
	$(MAKE) ndk-build
	$(MAKE) ndk-ant

install.apk:
	adb $(ANDROIDHOST) install $(ANDROIDBUILDDIR)/bin/$(HOPAPKNAME)-debug.apk

click.apk:
	adb $(ANDROIDHOST) shell monkey -p fr.inria.hop 1

apk.common: $(ANDROIDBUILDDIR)/local.properties \
  $(ANDROIDBUILDDIR)/ant.properties \
  $(ANDROIDBUILDDIR)/build.xml \
  $(ANDROIDBUILDDIR)/Android.mk \
  $(ANDROIDBUILDDIR)/AndroidManifest.xml \
  $(ANDROIDBUILDDIR)/project.properties

apk.res: $(ANDROIDBUILDDIR)/res \
  $(ANDROIDBUILDDIR)/res/values/strings.xml \
  $(ANDROIDBUILDDIR)/res/values/plugins.xml \
  $(ANDROIDBUILDDIR)/res/xml/preferences.xml

apk.jni: $(ANDROIDBUILDDIR)/jni
$(ANDROIDBUILDDIR)/jni: jni $(ANDROIDBUILDDIR)
	cp -r $< $@

apk.src: $(ANDROIDBUILDDIR)/src
$(ANDROIDBUILDDIR)/src: $(ANDROIDDIR)/src $(ANDROIDBUILDDIR)
	$(RM) -rf $@
	cp -r $(ANDROIDDIR)/src $@

apk.bigloo: $(ANDROIDBUILDDIR)/bigloo$(RELEASE)
	$(MAKE) apk.bigloo.configure
	$(MAKE) -C $(ANDROIDBUILDDIR)/bigloo$(RELEASE)
	$(MAKE) -C $(ANDROIDBUILDDIR)/bigloo$(RELEASE) install DESTDIR=$(ANDROIDBUILDDIR)

apk.bigloo.configure:
	(cd $(ANDROIDBUILDDIR)/bigloo$(RELEASE); \
         ./configure --os-android \
            --android-adb=adb \
            --cc=$(ANDROIDTOOLCHAIN)/bin/arm-linux-androideabi-gcc \
            --cflags="-pie -DBGL_GC_ROOTS" \
            --lflags=-pie \
            --libuvconfigureopt="--host=arm-linux-androideabi" \
            --stack-check=no \
            --prefix=$(ANDROIDHOPPREFIX) \
            --build-bindir=$(HOPBINDIR) \
            $(BIGLOOCONFIGUREOPT))

$(ANDROIDBUILDDIR)/bigloo$(RELEASE): $(REPOSITORY)/bigloo$(RELEASE).tar.gz
	mkdir -p $(ANDROIDBUILDDIR)
	(cd $(ANDROIDBUILDDIR); rm -rf $(ANDROIDBUILDDIR)/bigloo$(RELEASE))
	(cd $(ANDROIDBUILDDIR); tar xvf $< 2> /dev/null > /dev/null)

apk.hop: $(ANDROIDBUILDDIR)/$(HOPFULLNAME)
	$(MAKE) apk.hop.configure
	$(MAKE) -C $(ANDROIDBUILDDIR)/$(HOPFULLNAME)
	$(MAKE) -C $(ANDROIDBUILDDIR)/$(HOPFULLNAME) install DESTDIR=$(ANDROIDBUILDDIR)

apk.hop.configure:
	(cd $(ANDROIDBUILDDIR)/$(HOPFULLNAME); \
         ./configure \
            --bigloo=$(HOPBINDIR)/bigloo \
            --bigloolibdir=$(ANDROIDBUILDDIR)$(ANDROIDHOPPREFIX)/lib/bigloo/$(RELEASE) \
            --android \
            --hopc=$(HOPBINDIR)/hopc \
            --hop=$(HOPBINDIR)/hop \
            --prefix=$(ANDROIDHOPPREFIX) \
            --libdir=$(ANDROIDHOPPREFIX)/lib \
            $(HOPCONFIGUREOPT))

$(ANDROIDBUILDDIR)/$(HOPFULLNAME): $(REPOSITORY)/$(HOPFULLNAME).tar.gz
	(cd $(ANDROIDBUILDDIR); rm -rf $(ANDROIDBUILDDIR)/$(HOPFULLNAME))
	(cd $(ANDROIDBUILDDIR); tar xvf $<)

apk-clean:
	rm -rf $(ANDROIDBUILDDIR)
	rm -f $(HOPTMPDIR)/antandroid.sh
