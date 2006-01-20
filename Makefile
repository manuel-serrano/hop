#*=====================================================================*/
#*    serrano/prgm/project/hop/Makefile                                */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Sat Feb 19 12:25:16 2000                          */
#*    Last change :  Fri Jan 20 14:31:13 2006 (serrano)                */
#*    -------------------------------------------------------------    */
#*    The Makefile to build HOP.                                       */
#*=====================================================================*/
.PHONY: do 

do: build

#*---------------------------------------------------------------------*/
#*    Standard Bigloo configuration                                    */
#*---------------------------------------------------------------------*/
include etc/Makefile.hopconfig
include $(BIGLOOLIBDIR)/Makefile.config

#*---------------------------------------------------------------------*/
#*    POPULATION                                                       */
#*---------------------------------------------------------------------*/
POPULATION	= Makefile configure

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
#*    pop                                                              */
#*---------------------------------------------------------------------*/
pop:
	@ (cd runtime && $(MAKE) -s pop) && \
	  (cd src && $(MAKE) -s pop) && \
	  (cd etc && $(MAKE) -s pop) && \
	  (cd share && $(MAKE) -s pop) && \
	  (cd contribs && $(MAKE) -s pop) && \
          echo $(POPULATION)

#*---------------------------------------------------------------------*/
#*    install                                                          */
#*---------------------------------------------------------------------*/
install: install-init
	(cd runtime && $(MAKE) install) && \
	(cd src && $(MAKE) install) && \
	(cd share && $(MAKE) install) && \
	(cd weblets && $(MAKE) install) && \
	if [ -d private ]; then \
	   (cd private && $(MAKE) install) \
	fi

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
	   /bin/rm -rf work private && \
           cd .. && \
           tar cvfz hop$(HOPRELEASE).tar.gz hop$(HOPRELEASE) \
               --exclude=hop$(HOPRELEASE)/src/o && \
	   /bin/rm -rf /tmp/hop$(HOPRELEASE) && \
           mv hop$(HOPRELEASE).tar.gz $(DISTRIBDIR)); \
        fi

