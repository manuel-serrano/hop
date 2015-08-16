#*=====================================================================*/
#*    serrano/prgm/project/hop/3.0.x/Makefile                          */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Sat Feb 19 12:25:16 2000                          */
#*    Last change :  Sun Aug 16 17:42:03 2015 (serrano)                */
#*    -------------------------------------------------------------    */
#*    The Makefile to build HOP.                                       */
#*=====================================================================*/
.PHONY: do

do: build

#*---------------------------------------------------------------------*/
#*    Standard Bigloo configuration                                    */
#*---------------------------------------------------------------------*/
-include etc/Makefile.hopconfig
-include etc/Makefile.version
-include $(BIGLOOLIBDIR)/Makefile.config

#*---------------------------------------------------------------------*/
#*    POPULATION                                                       */
#*---------------------------------------------------------------------*/
POPULATION	= Makefile LICENSE README INSTALL INSTALL.jvm \
                  configure .hoprelease .hgignore .gitignore
POPDIRS		= runtime hopscheme scheme2js src hopc hopsh hopreplay hophz \
                  etc share arch \
                  weblets widget

#*---------------------------------------------------------------------*/
#*    build                                                            */
#*---------------------------------------------------------------------*/
.PHONY: bindir libdir lib widget share weblets bin \
  share-afile scheme2js \
  android

build: bindir libdir lib weblets widget $(BUILDSPECIFIC) bin share

bindir:
	mkdir -p bin

libdir:
	mkdir -p lib

bin: bindir hopc-bin src-bin hopsh-bin hopreplay-bin hophz-bin

hopc-bin: lib
	$(MAKE) -C hopc build

src-bin: lib widget $(BUILDSPECIFIC) 
	$(MAKE) -C src build

hopsh-bin: lib
	$(MAKE) -C hopsh build

hopreplay-bin: lib
	$(MAKE) -C hopreplay build

hophz-bin: lib hopc-bin widget
	$(MAKE) -C hophz build

lib: libdir scheme2js
	$(MAKE) -C hopscheme build
	$(MAKE) -C runtime build

widget: libdir hopc-bin share-afile
	$(MAKE) -C widget build

share-afile: scheme2js
	$(MAKE) -C share .afile

share: bin hopc-bin scheme2js
	$(MAKE) -C share build

weblets: lib hopc-bin
	$(MAKE) -C weblets build

scheme2js:
	$(MAKE) -C scheme2js build

build-android: lib
	$(MAKE) -C arch/android build

#*---------------------------------------------------------------------*/
#*    dep                                                              */
#*---------------------------------------------------------------------*/
dep:
	$(MAKE) -C scheme2js dep
	$(MAKE) -C hopscheme dep
	$(MAKE) -C runtime dep
	$(MAKE) -C src dep
	$(MAKE) -C hopsh dep
	$(MAKE) -C hopreplay dep

#*---------------------------------------------------------------------*/
#*    ude                                                              */
#*---------------------------------------------------------------------*/
ude:
	$(MAKE) -C scheme2js ude
	$(MAKE) -C hopscheme ude
	$(MAKE) -C runtime ude
	$(MAKE) -C src ude
	$(MAKE) -C hopsh ude
	$(MAKE) -C hopreplay ude

#*---------------------------------------------------------------------*/
#*    changelog                                                        */
#*---------------------------------------------------------------------*/
.PHONY: changelog

changelog:
	@ $(MAKE) log

#*---------------------------------------------------------------------*/
#*    install                                                          */
#*---------------------------------------------------------------------*/
install: install-quick install-share install-weblets $(INSTALLSPECIFIC)

install-share: hop-dirs
	$(MAKE) -C share install

install-weblets: hop-dirs
	$(MAKE) -C weblets install

install-quick: hop-dirs install-init
	$(MAKE) -C runtime install && \
	$(MAKE) -C widget install && \
	$(MAKE) -C scheme2js install && \
	$(MAKE) -C hopscheme install && \
	$(MAKE) -C src install && \
	$(MAKE) -C hopsh install && \
	$(MAKE) -C hopc install && \
	$(MAKE) -C hophz install && \
	$(MAKE) -C etc install

install-init: hop-dirs
	$(INSTALL) $(BUILDLIBDIR)/hop.init $(DESTDIR)$(HOPLIBDIR)/$(HOPFILDIR)/hop.init && \
        chmod $(MODFILE) $(DESTDIR)$(HOPLIBDIR)/$(HOPFILDIR)/hop.init;
	$(INSTALL) $(BUILDLIBDIR)/hopwidget.init $(DESTDIR)$(HOPLIBDIR)/$(HOPFILDIR)/hopwidget.init && \
        chmod $(MODFILE) $(DESTDIR)$(HOPLIBDIR)/$(HOPFILDIR)/hopwidget.init;
	$(INSTALL) $(BUILDLIBDIR)/scheme2js.init $(DESTDIR)$(HOPLIBDIR)/$(HOPFILDIR)/scheme2js.init && \
        chmod $(MODFILE) $(DESTDIR)$(HOPLIBDIR)/$(HOPFILDIR)/scheme2js.init;
	$(INSTALL) $(BUILDLIBDIR)/hopscheme.init $(DESTDIR)$(HOPLIBDIR)/$(HOPFILDIR)/hopscheme.init && \
        chmod $(MODFILE) $(DESTDIR)$(HOPLIBDIR)/$(HOPFILDIR)/hopscheme.init;

hop-dirs:
	if [ ! -d $(DESTDIR)$(HOPBINDIR) ]; then \
          mkdir -p $(DESTDIR)$(HOPBINDIR) \
            && chmod $(MODDIR) $(DESTDIR)$(HOPBINDIR); \
        fi
	if [ ! -d $(DESTDIR)$(HOPLIBDIR) ]; then \
          mkdir -p $(DESTDIR)$(HOPLIBDIR) \
            && chmod $(MODDIR) $(DESTDIR)$(HOPLIBDIR); \
        fi
	if [ ! -d $(DESTDIR)$(HOPSHAREDIR) ]; then \
	  mkdir -p $(DESTDIR)$(HOPSHAREDIR) \
            && chmod $(MODDIR) $(DESTDIR)$(HOPSHAREDIR); \
        fi
	if [ ! -d $(DESTDIR)$(HOPETCDIR) ]; then \
	  mkdir -p $(DESTDIR)$(HOPETCDIR) \
            && chmod $(MODDIR) $(DESTDIR)$(HOPETCDIR); \
        fi
	if [ ! -d $(DESTDIR)$(HOPLIBDIR)/hop ]; then \
	  mkdir -p $(DESTDIR)$(HOPLIBDIR)/hop \
           && chmod $(MODDIR) $(DESTDIR)$(HOPLIBDIR)/hop; \
        fi
	if [ ! -d $(DESTDIR)$(HOPLIBDIR)/$(HOPFILDIR) ]; then \
	  mkdir -p $(DESTDIR)$(HOPLIBDIR)/$(HOPFILDIR) \
           && chmod $(MODDIR) $(DESTDIR)$(HOPLIBDIR)/$(HOPFILDIR); \
        fi
	if [ ! -d $(DESTDIR)$(HOPWEBLETSDIR) ]; then \
	  mkdir -p $(DESTDIR)$(HOPWEBLETSDIR) \
	   && chmod $(MODDIR) $(DESTDIR)$(HOPWEBLETSDIR); \
        fi

install-android: hop-dirs
	$(INSTALL) $(BUILDLIBDIR)/hopdroid.init $(DESTDIR)$(HOPLIBDIR)/$(HOPFILDIR)/hopdroid.init && \
        chmod $(MODFILE) $(DESTDIR)$(HOPLIBDIR)/$(HOPFILDIR)/hopdroid.init;
	$(MAKE) -C arch/android install

install-debian: hop-dirs
	mkdir -p $(DESTDIR)/etc/init.d
	$(INSTALL) $(BUILDDIR)/arch/debian/init.d/hop $(DESTDIR)/etc/init.d \
	  && chmod u+rx $(DESTDIR)/etc/init.d/hop

#*---------------------------------------------------------------------*/
#*    uninstall                                                        */
#*---------------------------------------------------------------------*/
uninstall:
	$(MAKE) -C etc uninstall
	$(MAKE) -C src uninstall
	$(MAKE) -C hopsh uninstall
	$(MAKE) -C runtime uninstall
	$(MAKE) -C widget uninstall
	$(MAKE) -C scheme2js uninstall
	$(MAKE) -C hopscheme uninstall
	if [ "$(DESTDIR)$(HOPLIBDIR)/$(HOPFILDIR)" != "/" ]; then \
	  $(RM) -rf $(DESTDIR)$(HOPLIBDIR)/$(HOPFILDIR); \
        fi

#*---------------------------------------------------------------------*/
#*    clean                                                            */
#*---------------------------------------------------------------------*/
clean-quick:
	$(MAKE) -C runtime clean
	$(MAKE) -C src clean
	$(MAKE) -C hopsh clean
	$(MAKE) -C hophz clean
	$(MAKE) -C hopc clean
	$(MAKE) -C hopreplay clean
	$(MAKE) -C weblets clean
	$(MAKE) -C widget clean
	$(MAKE) -C share clean

clean:
	$(MAKE) -C runtime clean
	$(MAKE) -C scheme2js clean
	$(MAKE) -C hopscheme clean
	$(MAKE) -C src clean
	$(MAKE) -C hopsh clean
	$(MAKE) -C hophz clean
	$(MAKE) -C hopc clean
	$(MAKE) -C hopreplay clean
	$(MAKE) -C etc clean
	$(MAKE) -C weblets clean
	$(MAKE) -C widget clean
	$(MAKE) -C share clean

devclean:
	$(MAKE) -C runtime devclean
	$(MAKE) -C src devclean
	$(MAKE) -C hopc devclean
	$(MAKE) -C hopsh devclean
	$(MAKE) -C hophz devclean
	$(MAKE) -C hopreplay devclean
	$(MAKE) -C widget devclean
	$(MAKE) -C share devclean

distclean: clean 
	$(MAKE) -C runtime distclean
	$(MAKE) -C src distclean
	$(MAKE) -C hopc distclean
	$(MAKE) -C hopsh distclean
	$(MAKE) -C hophz distclean
	$(MAKE) -C hopreplay distclean
	$(MAKE) -C widget distclean
	$(MAKE) -C share distclean
	$(RM) -f etc/Makefile.hopconfig
	$(RM) -f etc/hop.man
	$(RM) -f etc/hopsh.man
	$(RM) -f etc/hophz.man
	$(RM) -f etc/hopreplay.man
	$(RM) -f lib/hop.init
	$(RM) -f lib/scheme2js.init
	$(RM) -f lib/hopscheme.init
	$(RM) -f runtime/configure_android.sch
	$(RM) -f runtime/configure_macosx.sch
	$(RM) -f runtime/configure_noarch.sch
	$(RM) -f config.status

cleanall: distclean

#*---------------------------------------------------------------------*/
#*    distrib:                                                         */
#*---------------------------------------------------------------------*/
.PHONY: distrib newdistrib distrib-inc-version distrib-sans-version
.PHONY: distrib-tmp distrib-pre distrib-native distrib-jvm

distrib:
	$(MAKE) distrib-sans-version

newdistrib:
	$(MAKE) distrib-inc-version
	$(MAKE) distrib-sans-version

distrib-inc-version:
	if [ -d $(HOPTMPDIR)/hop-tmp ]; then \
          echo "*** ERROR: $(HOPTMPDIR)/hop-tmp exists!"; \
          exit 1; \
        elif [ -d $(HOPTMPDIR)/hop-$(HOPRELEASE) ]; then \
          echo "*** ERROR: $(HOPTMPDIR)/hop-$(HOPRELEASE) exists!"; \
          exit 1; \
        else \
          version=$(HOPRELEASE); \
          devel=$(HOPDEVEL); \
          if [ -f .hoprelease ]; then \
             . ./.hoprelease; \
             rm -f .hoprelease; \
          fi; \
          if [ "$$devel " = " " ]; then \
            distrib=$$version; \
            min=; \
          else \
            if [ "$$version " != "$$major " ]; then \
              min=1; \
            else \
              if [ "$$devel " == "$$state " ]; then \
                min=`expr $$minor + 1`; \
              else \
                min=1; \
              fi; \
            fi; \
            distrib=$$version-$$devel$$min; \
          fi; \
          echo "#!/bin/sh" > .hoprelease; \
          echo "major=$$version" >> .hoprelease; \
          echo "state=$$devel" >> .hoprelease; \
          echo "minor=$$min" >> .hoprelease; \
          chmod a+rx .hoprelease; \
	  $(MAKE) revision LOGMSG="New distrib $$version-$$devel$$min"; \
        fi

distrib-sans-version: distrib-native # distrib-jvm

distrib-pre:
	(version=$(HOPRELEASE); \
         devel=$(HOPDEVEL); \
          if [ -f .hoprelease ]; then \
             . ./.hoprelease; \
          fi; \
          if [ "$$devel " = " " ]; then \
            distrib=$$version; \
            minor=; \
          else \
            distrib=$$version-$$devel$$minor; \
          fi; \
          (cd weblets/home && make) && make OPT="-m 'build $$distrib'" revision || exit 0)

distrib-native: distrib-tmp
	(version=$(HOPRELEASE); \
         devel=$(HOPDEVEL); \
          if [ -f .hoprelease ]; then \
             . ./.hoprelease; \
          fi; \
          if [ "$$devel " = " " ]; then \
            distrib=$$version; \
            minor=; \
          else \
            distrib=$$version-$$devel$$minor; \
          fi; \
	  echo "Building hop-$(HOPRELEASE).tar.gz..." && \
          $(MAKE) clone CLONEDIR=$(HOPTMPDIR)/hop-tmp && \
	  $(MAKE) changelog > $(HOPTMPDIR)/hop-tmp/ChangeLog && \
	  $(RM) -rf $(HOPTMPDIR)/hop-tmp/weblets/home/talks && \
	  $(RM) -rf $(HOPTMPDIR)/hop-tmp/weblets/home/videos && \
          mv $(HOPTMPDIR)/hop-tmp $(HOPTMPDIR)/hop-$$distrib && \
          (cd $(HOPTMPDIR)/hop-$$distrib && \
           ./configure && \
           $(MAKE) predistrib && \
           $(MAKE) distclean) && \
          tar cvfz hop-$$distrib.tar.gz --exclude .hg -C $(HOPTMPDIR) hop-$$distrib && \
          $(RM) -rf $(HOPTMPDIR)/hop-$$distrib && \
          if [ $(HOPDISTRIBDIR) != "." ]; then \
            if [ $(HOPDISTRIBDIR) != "" ]; then \
              $(RM) -f $(HOPDISTRIBDIR)/hop-$(HOPRELEASE)*.tar.gz && \
              mv hop-$$distrib.tar.gz $(HOPDISTRIBDIR); \
            fi \
          fi) || exit 1

distrib-jvm: distrib-tmp
	(version=$(HOPRELEASE); \
         devel=$(HOPDEVEL); \
          if [ -f .hoprelease ]; then \
             . ./.hoprelease; \
          fi; \
          if [ "$$devel " = " " ]; then \
            distrib=$$version; \
            minor=; \
          else \
            distrib=$$version-$$devel$$minor; \
          fi; \
	  echo "Building hop-$(HOPRELEASE).jar..."; \
          $(MAKE) clone CLONEDIR=$(HOPTMPDIR)/hop-tmp && \
          mv $(HOPTMPDIR)/hop-tmp $(HOPTMPDIR)/hop-$$distrib && \
          (cd $(HOPTMPDIR)/hop-$$distrib && \
           ./configure --backend=jvm && \
           $(MAKE) && \
	   $(MAKE) changelog > ChangeLog && \
	   $(RM) -rf $(HOPTMPDIR)/hop-$$distrib/weblets/home/talks && \
	   $(RM) -rf $(HOPTMPDIR)/hop-$$distrib/weblets/home/videos && \
           $(RM) -f $(HOPDISTRIBDIR)/java/hop-$(HOPRELEASE)*.jar && \
           mv bin/hop.jar $(HOPDISTRIBDIR)/java/hop-$$distrib.jar) && \
          $(RM) -rf $(HOPTMPDIR)/hop-$$distrib) || exit 1

distrib-tmp:
	if [ -d $(HOPTMPDIR)/hop-tmp ]; then \
          echo "*** ERROR: $(HOPTMPDIR)/hop-tmp exists!"; \
          exit 1; \
        elif [ -d $(HOPTMPDIR)/hop-$(HOPRELEASE) ]; then \
          echo "*** ERROR: $(HOPTMPDIR)/hop-$(HOPRELEASE) exists!"; \
          exit 1; \
        fi

#*---------------------------------------------------------------------*/
#*    predistrib:                                                      */
#*---------------------------------------------------------------------*/
.PHONY: predistrib

predistrib:
	$(MAKE)
	$(MAKE) -C widget predistrib
	$(MAKE) -C share predistrib
	$(MAKE) -C hophz predistrib
