#*=====================================================================*/
#*    serrano/prgm/project/hop/Makefile                                */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Sat Feb 19 12:25:16 2000                          */
#*    Last change :  Mon Apr  3 07:04:10 2006 (serrano)                */
#*    -------------------------------------------------------------    */
#*    The Makefile to build HOP.                                       */
#*=====================================================================*/
.PHONY: do 

do: build

#*---------------------------------------------------------------------*/
#*    Standard Bigloo configuration                                    */
#*---------------------------------------------------------------------*/
include etc/Makefile.hopconfig
include etc/Makefile.version
include $(BIGLOOLIBDIR)/Makefile.config

#*---------------------------------------------------------------------*/
#*    POPULATION                                                       */
#*---------------------------------------------------------------------*/
POPULATION	= Makefile configure
POPDIRS		= runtime hopscheme scheme2js hopwiki src \
                  etc share \
                  contribs weblets demos

#*---------------------------------------------------------------------*/
#*    build                                                            */
#*---------------------------------------------------------------------*/
.PHONY: bindir libdir bin lib weblets

build: showflags bindir libdir bin lib weblets

bindir:
	mkdir -p bin

libdir:
	mkdir -p lib

bin: lib
	(cd src && $(MAKE) build)

lib:
	(cd runtime && $(MAKE) build)
	(cd scheme2js && $(MAKE) build)
	(cd hopscheme && $(MAKE) build)
	(cd hopwiki && $(MAKE) build)

weblets:
	(cd weblets && $(MAKE) build)

#*---------------------------------------------------------------------*/
#*    dep                                                              */
#*---------------------------------------------------------------------*/
dep:
	(cd runtime; $(MAKE) dep)
	(cd scheme2js; $(MAKE) dep)
	(cd hopscheme; $(MAKE) dep)
	(cd hopwiki; $(MAKE) dep)
	(cd src; $(MAKE) dep)

#*---------------------------------------------------------------------*/
#*    ude                                                              */
#*---------------------------------------------------------------------*/
ude:
	(cd runtime; $(MAKE) ude)
	(cd scheme2js; $(MAKE) ude)
	(cd hopscheme; $(MAKE) ude)
	(cd hopwiki; $(MAKE) ude)
	(cd src; $(MAKE) ude)

#*---------------------------------------------------------------------*/
#*    install                                                          */
#*---------------------------------------------------------------------*/
install: install-quick
	(cd weblets && $(MAKE) install)
	(cd demos && $(MAKE) install)

install-quick: hop-dirs install-init etc-hoprc
	(cd runtime && $(MAKE) install) && \
	(cd scheme2js && $(MAKE) install) && \
	(cd hopscheme && $(MAKE) install) && \
	(cd hopwiki && $(MAKE) install) && \
	(cd src && $(MAKE) install) && \
	(cd share && $(MAKE) install)

install-init: hop-dirs
	cp $(BUILDLIBDIR)/hop.init $(DESTDIR)$(HOPFILDIR)/hop.init && \
        chmod $(BMASK) $(DESTDIR)$(HOPFILDIR)/hop.init;
	cp $(BUILDLIBDIR)/scheme2js.init $(DESTDItR)$(HOPFILDIR)/scheme2js.init && \
        chmod $(BMASK) $(DESTDIR)$(HOPFILDIR)/scheme2js.init;
	cp $(BUILDLIBDIR)/hopscheme.init $(DESTDIR)$(HOPFILDIR)/hopscheme.init && \
        chmod $(BMASK) $(DESTDIR)$(HOPFILDIR)/hopscheme.init;
	cp $(BUILDLIBDIR)/hopwiki.init $(DESTDIR)$(HOPFILDIR)/hopwiki.init && \
        chmod $(BMASK) $(DESTDIR)$(HOPFILDIR)/hopwiki.init;

hop-dirs:
	mkdir -p $(DESTDIR)$(HOPBINDIR)
	mkdir -p $(DESTDIR)$(HOPLIBDIR)
	mkdir -p $(DESTDIR)$(HOPSHAREDIR)
	mkdir -p $(DESTDIR)$(HOPFILDIR)
	mkdir -p $(DESTDIR)$(HOPWEBLETSDIR)
	mkdir -p $(DESTDIR)$(HOPCONTRIBSDIR)
	mkdir -p $(DESTDIR)$(HOPETCDIR)

etc-hoprc:
	if [ -d $(DESTDIR)$(HOPETCDIR) ]; then \
          if [ ! -f $(DESTDIR)$(HOPETCDIR)/hoprc.hop ]; then \
            install -m a+r etc/hoprc.hop $(DESTDIR)$(HOPETCDIR); \
          fi; \
        fi

#*---------------------------------------------------------------------*/
#*    uninstall                                                        */
#*---------------------------------------------------------------------*/
uninstall:
	(cd src; $(MAKE) uninstall)
	(cd runtime; $(MAKE) uninstall)
	(cd scheme2js; $(MAKE) uninstall)
	(cd hopscheme; $(MAKE) uninstall)
	(cd hopwiki; $(MAKE) uninstall)
	(cd demos; $(MAKE) uninstall)
	/bin/rm -rf $(HOPFILDIR)

#*---------------------------------------------------------------------*/
#*    clean                                                            */
#*---------------------------------------------------------------------*/
clean:
	(cd runtime; $(MAKE) clean)
	(cd scheme2js; $(MAKE) clean)
	(cd hopscheme; $(MAKE) clean)
	(cd hopwiki; $(MAKE) clean)
	(cd src; $(MAKE) clean)

devclean:
	(cd runtime; $(MAKE) devclean)
	(cd src; $(MAKE) devclean)

distclean: clean devclean
	/bin/rm -f etc/Makefile.hopconfig
	/bin/rm -f lib/hop.init
	/bin/rm -f lib/scheme2js.init
	/bin/rm -f lib/hopscheme.init
	/bin/rm -f lib/hopwiki.init

cleanall: distclean

#*---------------------------------------------------------------------*/
#*    distrib:                                                         */
#*---------------------------------------------------------------------*/
distrib:
	if [ -f $(HOPTMPDIR)/hop ]; then \
          echo "*** ERROR: $(HOPTMPDIR)/hop exists!"; \
          exit 1; \
        elif [ -f $(HOPTMPDIR)/hop$(HOPRELEASE) ]; then \
          echo "*** ERROR: $(HOPTMPDIR)/hop$(HOPRELEASE) exists!"; \
          exit 1; \
        else \
          ver=$(HOPRELEASE); \
          dev=$(HOPDEVEL); \
          min=1; \
          if [ -f .hoprelease ]; then \
             . .hoprelease; \
             rm -f .hoprelease; \
             if [ $$ver = $$version -a $$dev == $$devel ]; then \
               min=`expr $$minor + 1`; \
             fi \
          fi && \
          if [ "$$dev " = " " ]; then \
            distrib=$$ver; \
          else \
            distrib=$$ver-$$dev$$min; \
          fi && \
          echo "version=$$ver" > .hoprelease; \
          echo "devel=$$dev" >> .hoprelease; \
          echo "minor=$$min" >> .hoprelease; \
          $(MAKE) clone DESTDIR=$(HOPTMPDIR)/hop && \
          mv $(HOPTMPDIR)/hop $(HOPTMPDIR)/hop-$$distrib && \
          tar cvfz hop-$$distrib.tar.gz --exclude .hg -C $(HOPTMPDIR) hop-$$distrib && \
          $(RM) -rf $(HOPTMPDIR)/hop-$$distrib && \
          if [ $(HOPDISTRIBDIR) != "." ]; then \
            if [ $(HOPDISTRIBDIR) != "" ]; then \
              /bin/rm -f $(HOPDISTRIBDIR)/hop-$(HOPRELEASE)*.tar.gz && \
              mv hop-$$distrib.tar.gz $(HOPDISTRIBDIR); \
            fi \
          fi \
        fi

