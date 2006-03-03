#*=====================================================================*/
#*    serrano/prgm/project/hop/Makefile                                */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Sat Feb 19 12:25:16 2000                          */
#*    Last change :  Fri Mar  3 16:00:44 2006 (eg)                     */
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
POPDIRS		= runtime src etc share contribs weblets demos

#*---------------------------------------------------------------------*/
#*    build                                                            */
#*---------------------------------------------------------------------*/
.PHONY: bindir libdir bin lib

build: showflags bindir libdir bin lib

bindir:
	mkdir -p bin

libdir:
	mkdir -p lib

bin: lib
	(cd src && $(MAKE) build)

lib:
	(cd runtime && $(MAKE) build)

#*---------------------------------------------------------------------*/
#*    dep                                                              */
#*---------------------------------------------------------------------*/
dep:
	(cd runtime; $(MAKE) dep)
	(cd src; $(MAKE) dep)

#*---------------------------------------------------------------------*/
#*    ude                                                              */
#*---------------------------------------------------------------------*/
ude:
	(cd runtime; $(MAKE) ude)
	(cd src; $(MAKE) ude)

#*---------------------------------------------------------------------*/
#*    install                                                          */
#*---------------------------------------------------------------------*/
install: install-init
	(cd runtime && $(MAKE) install) && \
	(cd src && $(MAKE) install) && \
	(cd share && $(MAKE) install) && \
	(cd weblets && $(MAKE) install)
	(cd demos && $(MAKE) install)

install-init: $(DESTDIR)$(HOPFILDIR)
	cp $(BUILDLIBDIR)/hop.init $(DESTDIR)$(HOPFILDIR)/hop.init && \
        chmod $(BMASK) $(DESTDIR)$(HOPFILDIR)/hop.init;

$(DESTDIR)$(HOPFILDIR):
	mkdir -p $(DESTDIR)$(HOPFILDIR)

#*---------------------------------------------------------------------*/
#*    uninstall                                                        */
#*---------------------------------------------------------------------*/
uninstall:
	(cd src; $(MAKE) uninstall)
	(cd runtime; $(MAKE) uninstall)
	(cd demos; $(MAKE) uninstall)
	/bin/rm -rf $(HOPFILDIR)

#*---------------------------------------------------------------------*/
#*    clean                                                            */
#*---------------------------------------------------------------------*/
clean:
	(cd runtime; $(MAKE) clean)
	(cd src; $(MAKE) clean)

devclean:
	(cd runtime; $(MAKE) devclean)
	(cd src; $(MAKE) devclean)

distclean: clean devclean
	/bin/rm -f etc/Makefile.hopconfig
	/bin/rm -f lib/hop.init

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
          $(MAKE) clone DESTDIR=$(HOPTMPDIR)/hop && \
          mv $(HOPTMPDIR)/hop $(HOPTMPDIR)/hop-$(HOPRELEASE) && \
          tar cvfz hop-$(HOPRELEASE).tar.gz --exclude .hg -C $(HOPTMPDIR) hop-$(HOPRELEASE) && \
          $(RM) -rf $(HOPTMPDIR)/hop-$(HOPRELEASE) && \
          if [ $(HOPDISTRIBDIR) != "." ]; then \
            if [ $(HOPDISTRIBDIR) != "" ]; then \
              mv hop-$(HOPRELEASE).tar.gz $(HOPDISTRIBDIR); \
            fi \
          fi \
        fi

