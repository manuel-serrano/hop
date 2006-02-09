#*=====================================================================*/
#*    serrano/prgm/project/hop/Makefile                                */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Sat Feb 19 12:25:16 2000                          */
#*    Last change :  Wed Feb  8 19:04:56 2006 (serrano)                */
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
POPDIRS		= runtime src etc share contribs weblets

#*---------------------------------------------------------------------*/
#*    build                                                            */
#*---------------------------------------------------------------------*/
build: showflags
	mkdir -p bin
	mkdir -p lib
	(cd runtime && $(MAKE) build)
	(cd src && $(MAKE) build)

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
	/bin/rm -rf $(FILDIR)

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
	@ if [ -f /tmp/hop ]; then \
          echo "*** ERROR: /tmp/hop exists!"; \
          exit 1; \
        else \
          (cp -r ../hop /tmp/hop$(HOPRELEASE) && \
           cd /tmp/hop$(HOPRELEASE) && \
           ./configure && \
           make devclean && \
	   find . -name '*~' -exec /bin/rm {} \; && \
           /bin/rm -f etc/Makefile.hopconfig && \
	   /bin/rm -rf work private .hg && \
           cd .. && \
           tar cvfz hop$(HOPRELEASE).tar.gz hop$(HOPRELEASE) \
               --exclude=hop$(HOPRELEASE)/src/o \
               --exclude=hop$(HOPRELEASE)/runtime/o \
               --exclude=hop$(HOPRELEASE)/lib \
               --exclude=hop$(HOPRELEASE)/.hgignore \
               --exclude=hop$(HOPRELEASE)/.hg && \
	   /bin/rm -rf /tmp/hop$(HOPRELEASE) && \
           mv hop$(HOPRELEASE).tar.gz $(DISTRIBDIR)); \
        fi

