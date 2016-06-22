#*=====================================================================*/
#*    serrano/prgm/project/hop/3.1.x/arch/android/Makefile             */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Tue Oct 12 11:53:03 2010                          */
#*    Last change :  Wed Jun 22 10:17:26 2016 (serrano)                */
#*    Copyright   :  2010-16 Manuel Serrano                            */
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
ANDROIDBUILDDIR = /tmp/build.hop.android
ANDROIDROOT = /opt/android
ANDROIDSDK = $(ANDROIDROOT)/android-sdk-linux
ANDROIDNDK = $(ANDROIDROOT)/android-ndk-r9d
ANDROIDTOOLCHAIN = $(ANDROIDROOT)/toolchain
ANDROIDSDKVERSION = 21
ANDROIDABI = armeabi
ANDROIDACTIONMODE = debug
ANDROIDHOST = -d

ANDROIDANT = ant

# hop androin configuration
ANDROIDHOPPORT = 8080
ANDROIDHOPTHREAD = 6
ANDROIDHOPZEROCONF = true
ANDROIDHOPDEBUG = false
ANDROIDHOPURL = http://hop.inria.fr
ANDROIDHOPCONSOLELOG=false
ANDROIDHOPDEBUGLEVEL = 0
ANDROIDHOPVERBOSELEVEL = -v
ANDROIDHOPPREFIX = /data/data/fr.inria.hop/assets

# global configuration
REPOSITORY = $(DISTRIBDIR)

HOPMINOR = -$(HOPDEVEL)1
HOPFULLNAME = hop-$(HOPRELEASE)$(HOPMINOR)

#*---------------------------------------------------------------------*/
#*    Sources                                                          */
#*---------------------------------------------------------------------*/
ANDROID_BUILD = Android.mk build.xml project.properties.in hop.keystore \
  AndroidManifest.xml.in local.properties.in ant.properties.in proguard.cfg

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
  res/layout/main.xml \
  res/layout/about.xml \
  res/layout/info.xml \
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
  res/menu/hop_menu.xml

ANDROID_JAVA = Hop.java HopConfigurer.java HopInstaller.java HopUiUtils.java \
  HopExec.java HopLauncher.java HopService.java HopBootReceiver.java \
  HopDroid.java HopPlugin.java HopPluginInit.java HopPluginLocale.java \
  HopPluginMusicPlayer.java HopPluginVibrate.java HopPluginSensor.java \
  HopPluginSms.java HopPluginContact.java HopPluginWifi.java \
  HopPluginBattery.java HopPluginTts.java HopPluginMediaAudio.java \
  HopPluginCall.java HopSms.java HopAndroidAudioRecorder.java \
  HopPluginBuild.java HopPluginConnectivity.java HopSettings.java \
  HopPluginZeroconf.java HopZeroconf.java HopJmDns.java HopNsdManager.java \
  HopPluginSystem.java

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

$(ANDROIDBUILDDIR)/local.properties $(ANDROIDBUILDDIR)/ant.properties:
	cat $< \
           | sed -e "s|@ANDROIDSDK@|$(ANDROIDSDK)|" \
                 -e "s|@BASEDIR@|$(BUILDDIR)/arch/android|" \
           > $@ || exit 1

$(ANDROIDBUILDDIR)/local.properties: local.properties.in $(ANDROIDBUILDDIR)
$(ANDROIDBUILDDIR)/ant.properties: ant.properties.in $(ANDROIDBUILDDIR)

$(ANDROIDBUILDDIR)/build.xml $(ANDROIDBUILDDIR)/Android.mk:
	cp $< $@

$(ANDROIDBUILDDIR)/build.xml: build.xml $(ANDROIDBUILDDIR)
$(ANDROIDBUILDDIR)/Android.mk: Android.mk $(ANDROIDBUILDDIR)

$(ANDROIDBUILDDIR)/AndroidManifest.xml $(ANDROIDBUILDDIR)/project.properties:
	cat $< \
          | sed -e "s|@HOPVERSION@|$(HOPRELEASE)|" \
                -e "s|@HOPDEBUG@|$(ANDROIDHOPDEBUG)|" \
                -e "s|@ANDROIDSDKVERSION@|$(ANDROIDSDKVERSION)|" \
                -e "s|@HOPPORT@|$(ANDROIDHOPPORT)|" \
                -e "s|@HOPTHREADS@|$(ANDROIDHOPTHREADS)|" \
                -e "s|@HOPZEROCONF@|$(ANDROIDHOPZEROCONF)|" \
                -e "s|@HOPURL@|$(ANDROIDHOPURL)|" > $@

$(ANDROIDBUILDDIR)/AndroidManifest.xml: AndroidManifest.xml.in $(ANDROIDBUILDDIR)
$(ANDROIDBUILDDIR)/project.properties: project.properties.in $(ANDROIDBUILDDIR)

#*---------------------------------------------------------------------*/
#*    res                                                              */
#*---------------------------------------------------------------------*/
$(ANDROIDBUILDDIR)/res: res $(ANDROIDBUILDDIR)
	$(RM) -rf $@
	cp -r $< $@

$(ANDROIDBUILDDIR)/res/values/strings.xml $(ANDROIDBUILDDIR)/res/xml/preferences.xml:
	rm -f $@.in
	cat $< \
          | sed -e "s|@HOPVERSION@|$(HOPRELEASE)|" \
                -e "s|@HOPPORT@|$(ANDROIDHOPPORT)|" \
                -e "s|@HOPTHREADS@|$(ANDROIDHOPTHREADS)|" \
                -e "s|@HOPCONSOLELOG@|$(ANDROIDHOPCONSOLELOG)|" \
                -e "s|@HOPDEBUGLEVEL@|$(ANDROIDHOPDEBUGLEVEL)|" \
                -e "s|@HOPVERBOSELEVEL@|$(ANDROIDHOPVERBOSELEVEL)|" \
                -e "s|@HOPZEROCONF@|$(ANDROIDHOPZEROCONF)|" \
                -e "s|@HOPPREFIX@|$(ANDROIDHOPPREFIX)|" \
                -e "s|@HOPURL@|$(ANDROIDHOPURL)|" > $@

$(ANDROIDBUILDDIR)/res/values/strings.xml: res/values/strings.xml.in \
   $(ANDROIDBUILDDIR)/res
$(ANDROIDBUILDDIR)/res/xml/preferences.xml: res/xml/preferences.xml.in \
   $(ANDROIDBUILDDIR)/res

#*---------------------------------------------------------------------*/
#*    jni                                                              */
#*---------------------------------------------------------------------*/
$(ANDROIDBUILDDIR)/jni: jni $(ANDROIDBUILDDIR)
	cp -r $< $@

#*---------------------------------------------------------------------*/
#*    src                                                              */
#*---------------------------------------------------------------------*/
$(ANDROIDBUILDDIR)/src: src $(ANDROIDBUILDDIR)
	$(RM) -rf $@
	cp -r $< $@

#*---------------------------------------------------------------------*/
#*    assets                                                           */
#*---------------------------------------------------------------------*/
$(ANDROIDBUILDDIR)/assets: \
  $(ANDROIDBUILDDIR)/assets/etc \
  $(ANDROIDBUILDDIR)/assets/bin \
  $(ANDROIDBUILDDIR)/assets/lib

$(ANDROIDBUILDDIR)/assets/etc:
	mkdir -p $(ANDROIDBUILDDIR)/assets
	cp -r $(ANDROIDBUILDDIR)$(ANDROIDHOPPREFIX)/etc $@

$(ANDROIDBUILDDIR)/assets/bin:
	mkdir -p $@
	cp $(ANDROIDBUILDDIR)$(ANDROIDHOPPREFIX)/bin/hop $@

$(ANDROIDBUILDDIR)/assets/lib: \
  $(ANDROIDBUILDDIR)/assets/lib/bigloo \
  $(ANDROIDBUILDDIR)/assets/lib/hop

$(ANDROIDBUILDDIR)/assets/lib/bigloo:
	mkdir -p $(ANDROIDBUILDDIR)/assets/lib
	$(RM) -rf $@
	cp -r $(ANDROIDBUILDDIR)$(ANDROIDHOPPREFIX)/lib/bigloo $@

$(ANDROIDBUILDDIR)/assets/lib/hop:
	mkdir -p $(ANDROIDBUILDDIR)/assets/lib
	$(RM) -rf $@
	cp -r $(ANDROIDBUILDDIR)$(ANDROIDHOPPREFIX)/lib/hop $@
	find $@ -name .afile -exec $(RM) {} \;

ndk-build: $(ANDROIDBUILDDIR)/assets
	$(ANDROIDNDK)/ndk-build -C $(ANDROIDBUILDDIR) V=1 || exit 1

ndk-ant: $(HOPTMPDIR)/antandroid.sh
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
.PHONY: apk install.apk check.apk apk.bigloo apk.hop apk.lib

apk: apk.common apk.res apk.jni apk.src apk.bigloo apk.hop
	$(MAKE) ndk-build
	$(MAKE) ndk-ant

install.apk:
	adb $(ANDROIDHOST) install -r $(ANDROIDBUILDDIR)/bin/hop-debug.apk

check.apk:
	adb $(ANDROIDHOST) shell monkey -p fr.inria.hop 1

apk.common: $(ANDROIDBUILDDIR)/local.properties \
  $(ANDROIDBUILDDIR)/ant.properties \
  $(ANDROIDBUILDDIR)/build.xml \
  $(ANDROIDBUILDDIR)/Android.mk \
  $(ANDROIDBUILDDIR)/AndroidManifest.xml \
  $(ANDROIDBUILDDIR)/project.properties

apk.res: $(ANDROIDBUILDDIR)/res \
  $(ANDROIDBUILDDIR)/res/values/strings.xml \
  $(ANDROIDBUILDDIR)/res/xml/preferences.xml

apk.jni: $(ANDROIDBUILDDIR)/jni

apk.src: $(ANDROIDBUILDDIR)/src

apk.bigloo: $(ANDROIDBUILDDIR)/bigloo$(RELEASE)
	$(MAKE) apk.bigloo.configure
	$(MAKE) -C $(ANDROIDBUILDDIR)/bigloo$(RELEASE)
	$(MAKE) -C $(ANDROIDBUILDDIR)/bigloo$(RELEASE) install DESTDIR=$(ANDROIDBUILDDIR)

apk.bigloo.configure:
	(cd $(ANDROIDBUILDDIR)/bigloo$(RELEASE); \
         ./configure --os-android \
            --android-adb=adb \
            --cc=$(ANDROIDTOOLCHAIN)/bin/arm-linux-androideabi-gcc \
            --cflags=-pie \
            --lflags=-pie \
            --libuvconfigureopt="--host=arm-linux-androideabi" \
            --stack-check=no \
            --prefix=$(ANDROIDHOPPREFIX) \
            --build-bindir=$(HOPBINDIR))

$(ANDROIDBUILDDIR)/bigloo$(RELEASE): $(REPOSITORY)/bigloo$(RELEASE).tar.gz
	(cd $(ANDROIDBUILDDIR); rm -rf $(ANDROIDBUILDDIR)/bigloo$(RELEASE))
	(cd $(ANDROIDBUILDDIR); tar xvf $<)

apk.hop: $(ANDROIDBUILDDIR)/$(HOPFULLNAME) apk.bigloo
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
            --libdir=$(ANDROIDHOPPREFIX)/lib)

$(ANDROIDBUILDDIR)/$(HOPFULLNAME): $(REPOSITORY)/$(HOPFULLNAME).tar.gz
	(cd $(ANDROIDBUILDDIR); rm -rf $(ANDROIDBUILDDIR)/$(HOPFULLNAME))
	(cd $(ANDROIDBUILDDIR); tar xvf $<)

apk-clean:
	rm -rf $(ANDROIDBUILDDIR)
	rm -f $(HOPTMPDIR)/antandroid.sh
