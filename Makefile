#*=====================================================================*/
#*    serrano/prgm/project/hop/Makefile                                */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Sat Feb 19 12:25:16 2000                          */
#*    Last change :  Fri Nov 23 12:21:16 2007 (serrano)                */
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
                  configure .hoprelease
POPDIRS		= runtime hopscheme scheme2js src hopsh \
                  etc share \
                  weblets # contribs

#*---------------------------------------------------------------------*/
#*    build                                                            */
#*---------------------------------------------------------------------*/
.PHONY: bindir libdir lib share weblets bin

build: showflags bindir libdir lib share weblets bin

bindir:
	mkdir -p bin

libdir:
	mkdir -p lib

bin: bindir src-bin hopsh-bin

src-bin: lib share
	(cd src && $(MAKE) build)

hopsh-bin: lib
	(cd hopsh && $(MAKE) build)

lib: libdir
	(cd runtime && $(MAKE) build)
	(cd scheme2js && $(MAKE) build)
	(cd hopscheme && $(MAKE) build)

share: lib
	(cd share && $(MAKE) build)

weblets: lib
	(cd weblets && $(MAKE) build)

#*---------------------------------------------------------------------*/
#*    dep                                                              */
#*---------------------------------------------------------------------*/
dep:
	(cd runtime; $(MAKE) dep)
	(cd scheme2js; $(MAKE) dep)
	(cd hopscheme; $(MAKE) dep)
	(cd src; $(MAKE) dep)
	(cd hopsh; $(MAKE) dep)

#*---------------------------------------------------------------------*/
#*    ude                                                              */
#*---------------------------------------------------------------------*/
ude:
	(cd runtime; $(MAKE) ude)
	(cd scheme2js; $(MAKE) ude)
	(cd hopscheme; $(MAKE) ude)
	(cd src; $(MAKE) ude)
	(cd hopsh; $(MAKE) ude)

#*---------------------------------------------------------------------*/
#*    install                                                          */
#*---------------------------------------------------------------------*/
install: install-quick
	(cd share && $(MAKE) install)
	(cd weblets && $(MAKE) install)

install-quick: hop-dirs install-init
	(cd runtime && $(MAKE) install) && \
	(cd scheme2js && $(MAKE) install) && \
	(cd hopscheme && $(MAKE) install) && \
	(cd src && $(MAKE) install) && \
	(cd hopsh && $(MAKE) install) && \
	(cd etc && $(MAKE) install)

install-init: hop-dirs
	cp $(BUILDLIBDIR)/hop.init $(DESTDIR)$(HOPLIBDIR)/$(HOPFILDIR)/hop.init && \
        chmod $(BMASK) $(DESTDIR)$(HOPLIBDIR)/$(HOPFILDIR)/hop.init;
	cp $(BUILDLIBDIR)/scheme2js.init $(DESTDIR)$(HOPLIBDIR)/$(HOPFILDIR)/scheme2js.init && \
        chmod $(BMASK) $(DESTDIR)$(HOPLIBDIR)/$(HOPFILDIR)/scheme2js.init;
	cp $(BUILDLIBDIR)/hopscheme.init $(DESTDIR)$(HOPLIBDIR)/$(HOPFILDIR)/hopscheme.init && \
        chmod $(BMASK) $(DESTDIR)$(HOPLIBDIR)/$(HOPFILDIR)/hopscheme.init;

hop-dirs:
	mkdir -p $(DESTDIR)$(HOPBINDIR)
	mkdir -p $(DESTDIR)$(HOPLIBDIR)
	mkdir -p $(DESTDIR)$(HOPLIBDIR)/$(HOPFILDIR)
	mkdir -p $(DESTDIR)$(HOPSHAREDIR)
	mkdir -p $(DESTDIR)$(HOPWEBLETSDIR)
	mkdir -p $(DESTDIR)$(HOPCONTRIBSDIR)
	mkdir -p $(DESTDIR)$(HOPETCDIR)

#*---------------------------------------------------------------------*/
#*    uninstall                                                        */
#*---------------------------------------------------------------------*/
uninstall:
	(cd etc; $(MAKE) uninstall)
	(cd src; $(MAKE) uninstall)
	(cd hopsh; $(MAKE) uninstall)
	(cd runtime; $(MAKE) uninstall)
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

clean:
	(cd runtime; $(MAKE) clean)
	(cd scheme2js; $(MAKE) clean)
	(cd hopscheme; $(MAKE) clean)
	(cd src; $(MAKE) clean)
	(cd hopsh; $(MAKE) clean)

devclean:
	(cd runtime; $(MAKE) devclean)
	(cd src; $(MAKE) devclean)
	(cd hopsh; $(MAKE) devclean)

distclean: clean devclean
	/bin/rm -f etc/Makefile.hopconfig
	/bin/rm -f lib/hop.init
	/bin/rm -f lib/scheme2js.init
	/bin/rm -f lib/hopscheme.init

cleanall: distclean

#*---------------------------------------------------------------------*/
#*    distrib:                                                         */
#*---------------------------------------------------------------------*/
distrib:
	if [ -d $(HOPTMPDIR)/hop ]; then \
          echo "*** ERROR: $(HOPTMPDIR)/hop exists!"; \
          exit 1; \
        elif [ -d $(HOPTMPDIR)/hop-$(HOPRELEASE) ]; then \
          echo "*** ERROR: $(HOPTMPDIR)/hop-$(HOPRELEASE) exists!"; \
          exit 1; \
        else \
          version=$(HOPRELEASE); \
          devel=$(HOPDEVEL); \
          if [ -f .hoprelease ]; then \
             . .hoprelease; \
             rm -f .hoprelease; \
          fi; \
          if [ "$$devel " = " " ]; then \
            distrib=$$version; \
            min=; \
          else \
            if [ $$version != $$major ]; then \
              min=1; \
            else \
              if [ $$devel == $$state ]; then \
                min=`expr $$minor + 1`; \
              else \
                min=1; \
              fi; \
            fi; \
            distrib=$$version-$$devel$$min; \
          fi; \
          echo "major=$$version" > .hoprelease; \
          echo "state=$$devel" >> .hoprelease; \
          echo "minor=$$min" >> .hoprelease; \
          (cd weblets/home && make) && make OPT="-m 'build $$distrib'" revision && \
	  echo "Building hop-$(HOPRELEASE).tar.gz..."; \
          $(MAKE) clone DESTDIR=$(HOPTMPDIR)/hop && \
          mv $(HOPTMPDIR)/hop $(HOPTMPDIR)/hop-$$distrib && \
          tar cvfz hop-$$distrib.tar.gz --exclude .hg -C $(HOPTMPDIR) hop-$$distrib && \
          $(RM) -rf $(HOPTMPDIR)/hop-$$distrib && \
          if [ $(HOPDISTRIBDIR) != "." ]; then \
            if [ $(HOPDISTRIBDIR) != "" ]; then \
              /bin/rm -f $(HOPDISTRIBDIR)/hop-$(HOPRELEASE)*.tar.gz && \
              mv hop-$$distrib.tar.gz $(HOPDISTRIBDIR); \
            fi \
          fi; \
	  echo "Building hop-$(HOPRELEASE).jar..."; \
          $(MAKE) clone DESTDIR=$(HOPTMPDIR)/hop && \
          mv $(HOPTMPDIR)/hop $(HOPTMPDIR)/hop-$$distrib && \
          (cd $(HOPTMPDIR)/hop-$$distrib && \
           ./configure --backend=jvm && \
           $(MAKE) && \
           /bin/rm -f $(HOPDISTRIBDIR)/hop-$(HOPRELEASE)*.jar && \
           mv bin/hop.jar $(HOPDISTRIBDIR)/hop-$$distrib.jar) && \
          $(RM) -rf $(HOPTMPDIR)/hop-$$distrib; \
        fi

# build a distribution without incrementing the version number
distrib-sans-version:
	if [ -d $(HOPTMPDIR)/hop ]; then \
          echo "*** ERROR: $(HOPTMPDIR)/hop exists!"; \
          exit 1; \
        elif [ -d $(HOPTMPDIR)/hop-$(HOPRELEASE) ]; then \
          echo "*** ERROR: $(HOPTMPDIR)/hop$(HOPRELEASE) exists!"; \
          exit 1; \
        else \
          version=$(HOPRELEASE); \
          devel=$(HOPDEVEL); \
          if [ -f .hoprelease ]; then \
             . .hoprelease; \
          fi; \
          if [ "$$devel " = " " ]; then \
            distrib=$$version; \
            minor=; \
          else \
            distrib=$$version-$$devel$$minor; \
          fi; \
          (cd weblets/home && make) && make OPT="-m 'build $$distrib'" revision && \
	  echo "Building hop-$(HOPRELEASE).tar.gz..."; \
          $(MAKE) clone DESTDIR=$(HOPTMPDIR)/hop && \
          mv $(HOPTMPDIR)/hop $(HOPTMPDIR)/hop-$$distrib && \
          tar cvfz hop-$$distrib.tar.gz --exclude .hg -C $(HOPTMPDIR) hop-$$distrib && \
          $(RM) -rf $(HOPTMPDIR)/hop-$$distrib && \
          if [ $(HOPDISTRIBDIR) != "." ]; then \
            if [ $(HOPDISTRIBDIR) != "" ]; then \
              /bin/rm -f $(HOPDISTRIBDIR)/hop-$(HOPRELEASE)*.tar.gz && \
              mv hop-$$distrib.tar.gz $(HOPDISTRIBDIR); \
            fi \
          fi; \
	  echo "Building hop-$(HOPRELEASE).jar..."; \
          $(MAKE) clone DESTDIR=$(HOPTMPDIR)/hop && \
          mv $(HOPTMPDIR)/hop $(HOPTMPDIR)/hop-$$distrib && \
          (cd $(HOPTMPDIR)/hop-$$distrib && \
           ./configure --backend=jvm && \
           $(MAKE) && \
           /bin/rm -f $(HOPDISTRIBDIR)/hop-$(HOPRELEASE)*.jar && \
           mv bin/hop.jar $(HOPDISTRIBDIR)/hop-$$distrib.jar) && \
          $(RM) -rf $(HOPTMPDIR)/hop-$$distrib; \
        fi

