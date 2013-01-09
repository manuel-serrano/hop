#*=====================================================================*/
#*    serrano/prgm/project/hop/2.4.x/arch/android/Makefile             */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Tue Oct 12 11:53:03 2010                          */
#*    Last change :  Wed Jan  9 07:26:27 2013 (serrano)                */
#*    Copyright   :  2010-13 Manuel Serrano                            */
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
  res/layout/main.xml res/layout/about.xml res/layout/info.xml \
  res/xml/preferences.xml \
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
