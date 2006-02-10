#*=====================================================================*/
#*    serrano/prgm/project/hop/Makefile                                */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Sat Feb 19 12:25:16 2000                          */
#*    Last change :  Fri Feb 10 07:19:09 2006 (serrano)                */
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
	@ if [ -f $(HOPTMPDIR)/hop ]; then \
          echo "*** ERROR: $(HOPTMPDIR)/hop exists!"; \
          exit 1; \
        else \
          $(MAKE) clone && tar -C $(HOPTMPDIR) cvfz hop$(HOPRELEASE).tar.gz $(HOPTMPDIR)/hop \
        fi

