#*=====================================================================*/
#*    serrano/prgm/project/hop/2.0.x/Makefile                          */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Sat Feb 19 12:25:16 2000                          */
#*    Last change :  Mon Dec 14 07:35:27 2009 (serrano)                */
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
                  configure .hoprelease .hgignore
POPDIRS		= runtime hopscheme scheme2js src hopc hopsh hopreplay \
                  etc share arch \
                  weblets widget

#*---------------------------------------------------------------------*/
#*    build                                                            */
#*---------------------------------------------------------------------*/
.PHONY: bindir libdir lib widget share weblets bin share-afile scheme2js

build: showflags bindir libdir lib weblets widget bin share $(BUILD-SPECIFIC)

bindir:
	mkdir -p bin

libdir:
	mkdir -p lib

bin: bindir hopc-bin src-bin hopsh-bin hopreplay-bin

hopc-bin: lib
	(cd hopc && $(MAKE) build)

src-bin: lib widget
	(cd src && $(MAKE) build)

hopsh-bin: lib
	(cd hopsh && $(MAKE) build)

hopreplay-bin: lib
	(cd hopreplay && $(MAKE) build)

lib: libdir scheme2js
	(cd hopscheme && $(MAKE) build)
	(cd runtime && $(MAKE) build)

widget: libdir hopc-bin share-afile
	(cd widget && $(MAKE) build)

share-afile: scheme2js
	(cd share && $(MAKE) .afile)

share: bin hopc-bin scheme2js
	(cd share && $(MAKE) build)

weblets: lib
	(cd weblets && $(MAKE) build)

scheme2js:
	(cd scheme2js && $(MAKE) build)

#*---------------------------------------------------------------------*/
#*    dep                                                              */
#*---------------------------------------------------------------------*/
dep:
	(cd scheme2js; $(MAKE) dep)
	(cd hopscheme; $(MAKE) dep)
	(cd runtime; $(MAKE) dep)
	(cd src; $(MAKE) dep)
	(cd hopsh; $(MAKE) dep)
	(cd hopreplay; $(MAKE) dep)

#*---------------------------------------------------------------------*/
#*    ude                                                              */
#*---------------------------------------------------------------------*/
ude:
	(cd scheme2js; $(MAKE) ude)
	(cd hopscheme; $(MAKE) ude)
	(cd runtime; $(MAKE) ude)
	(cd src; $(MAKE) ude)
	(cd hopsh; $(MAKE) ude)
	(cd hopreplay; $(MAKE) ude)

#*---------------------------------------------------------------------*/
#*    changelog                                                        */
#*---------------------------------------------------------------------*/
.PHONY: changelog

changelog:
	@ $(MAKE) log

#*---------------------------------------------------------------------*/
#*    install                                                          */
#*---------------------------------------------------------------------*/
install: install-quick $(INSTALL-SPECIFIC) install-share install-weblets

install-share:
	(cd share && $(MAKE) install)

install-weblets:
	(cd weblets && $(MAKE) install)

install-quick: hop-dirs install-init
	(cd runtime && $(MAKE) install) && \
	(cd widget && $(MAKE) install) && \
	(cd scheme2js && $(MAKE) install) && \
	(cd hopscheme && $(MAKE) install) && \
	(cd src && $(MAKE) install) && \
	(cd hopsh && $(MAKE) install) && \
	(cd etc && $(MAKE) install)

install-init: hop-dirs
	$(INSTALL) $(BUILDLIBDIR)/hop.init $(DESTDIR)$(HOPLIBDIR)/$(HOPFILDIR)/hop.init && \
        chmod $(BMASK) $(DESTDIR)$(HOPLIBDIR)/$(HOPFILDIR)/hop.init;
	$(INSTALL) $(BUILDLIBDIR)/hopwidget.init $(DESTDIR)$(HOPLIBDIR)/$(HOPFILDIR)/hopwidget.init && \
        chmod $(BMASK) $(DESTDIR)$(HOPLIBDIR)/$(HOPFILDIR)/hopwidget.init;
	$(INSTALL) $(BUILDLIBDIR)/scheme2js.init $(DESTDIR)$(HOPLIBDIR)/$(HOPFILDIR)/scheme2js.init && \
        chmod $(BMASK) $(DESTDIR)$(HOPLIBDIR)/$(HOPFILDIR)/scheme2js.init;
	$(INSTALL) $(BUILDLIBDIR)/hopscheme.init $(DESTDIR)$(HOPLIBDIR)/$(HOPFILDIR)/hopscheme.init && \
        chmod $(BMASK) $(DESTDIR)$(HOPLIBDIR)/$(HOPFILDIR)/hopscheme.init;

hop-dirs:
	mkdir -p $(DESTDIR)$(HOPBINDIR) \
         && chmod $(BMASK) $(DESTDIR)$(HOPBINDIR)
	mkdir -p $(DESTDIR)$(HOPLIBDIR) \
         && chmod $(BMASK) $(DESTDIR)$(HOPLIBDIR)
	mkdir -p $(DESTDIR)$(HOPSHAREDIR) \
         && chmod $(BMASK) $(DESTDIR)$(HOPSHAREDIR)
	mkdir -p $(DESTDIR)$(HOPETCDIR) \
         && chmod $(BMASK) $(DESTDIR)$(HOPETCDIR)
	mkdir -p $(DESTDIR)$(HOPLIBDIR)/hop \
         && chmod $(BMASK) $(DESTDIR)$(HOPLIBDIR)/hop
	mkdir -p $(DESTDIR)$(HOPLIBDIR)/$(HOPFILDIR) \
         && chmod $(BMASK) $(DESTDIR)$(HOPLIBDIR)/$(HOPFILDIR)
	mkdir -p $(DESTDIR)$(HOPWEBLETSDIR) \
	 && chmod $(BMASK) $(DESTDIR)$(HOPWEBLETSDIR)

#*---------------------------------------------------------------------*/
#*    uninstall                                                        */
#*---------------------------------------------------------------------*/
uninstall:
	(cd etc; $(MAKE) uninstall)
	(cd src; $(MAKE) uninstall)
	(cd hopsh; $(MAKE) uninstall)
	(cd runtime; $(MAKE) uninstall)
	(cd widget; $(MAKE) uninstall)
	(cd scheme2js; $(MAKE) uninstall)
	(cd hopscheme; $(MAKE) uninstall)
	/bin/rm -rf $(DESTDIR)$(HOPLIBDIR)/$(HOPFILDIR)

#*---------------------------------------------------------------------*/
#*    clean                                                            */
#*---------------------------------------------------------------------*/
clean-quick:
	(cd runtime; $(MAKE) clean)
	(cd src; $(MAKE) clean)
	(cd hopsh; $(MAKE) clean)
	(cd hopc; $(MAKE) clean)
	(cd hopreplay; $(MAKE) clean)
	(cd weblets; $(MAKE) clean)
	(cd widget; $(MAKE) clean)
	(cd share; $(MAKE) clean)

clean:
	(cd runtime; $(MAKE) clean)
	(cd scheme2js; $(MAKE) clean)
	(cd hopscheme; $(MAKE) clean)
	(cd src; $(MAKE) clean)
	(cd hopsh; $(MAKE) clean)
	(cd hopc; $(MAKE) clean)
	(cd hopreplay; $(MAKE) clean)
	(cd etc; $(MAKE) clean)
	(cd weblets; $(MAKE) clean)
	(cd widget; $(MAKE) clean)
	(cd share; $(MAKE) clean)

devclean:
	(cd runtime; $(MAKE) devclean)
	(cd src; $(MAKE) devclean)
	(cd hopc; $(MAKE) devclean)
	(cd hopsh; $(MAKE) devclean)
	(cd hopreplay; $(MAKE) devclean)
	(cd widget; $(MAKE) devclean)
	(cd share; $(MAKE) devclean)

distclean: clean devclean
	/bin/rm -f etc/Makefile.hopconfig
	/bin/rm -f etc/hop.man
	/bin/rm -f etc/hopsh.man
	/bin/rm -f etc/hopreplay.man
	/bin/rm -f lib/hop.init
	/bin/rm -f lib/scheme2js.init
	/bin/rm -f lib/hopscheme.init

cleanall: distclean

#*---------------------------------------------------------------------*/
#*    distrib:                                                         */
#*---------------------------------------------------------------------*/
.PHONY: distrib newdistrib distrib-inc-version distrib-sans-version

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
        fi

distrib-sans-version:
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
          fi; \
          if [ "$$devel " = " " ]; then \
            distrib=$$version; \
            minor=; \
          else \
            distrib=$$version-$$devel$$minor; \
          fi; \
          (cd weblets/home && make) && make OPT="-m 'build $$distrib'" revision && \
	  echo "Building hop-$(HOPRELEASE).tar.gz..."; \
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
              /bin/rm -f $(HOPDISTRIBDIR)/hop-$(HOPRELEASE)*.tar.gz && \
              mv hop-$$distrib.tar.gz $(HOPDISTRIBDIR); \
            fi \
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
           /bin/rm -f $(HOPDISTRIBDIR)/java/hop-$(HOPRELEASE)*.jar && \
           mv bin/hop.jar $(HOPDISTRIBDIR)/java/hop-$$distrib.jar) && \
          $(RM) -rf $(HOPTMPDIR)/hop-$$distrib; \
        fi

#*---------------------------------------------------------------------*/
#*    predistrib:                                                      */
#*---------------------------------------------------------------------*/
.PHONY: predistrib

predistrib:
	$(MAKE)
	$(MAKE) -C widget predistrib
	$(MAKE) -C share predistrib
