#*=====================================================================*/
#*    serrano/prgm/project/hop/Makefile                                */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Sat Feb 19 12:25:16 2000                          */
#*    Last change :  Thu Mar 16 08:08:38 2006 (serrano)                */
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
POPDIRS		= runtime scheme2js src etc share contribs weblets demos

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
	(cd scheme2js && $(MAKE) build)

#*---------------------------------------------------------------------*/
#*    dep                                                              */
#*---------------------------------------------------------------------*/
dep:
	(cd runtime; $(MAKE) dep)
	(cd scheme2js; $(MAKE) dep)
	(cd src; $(MAKE) dep)

#*---------------------------------------------------------------------*/
#*    ude                                                              */
#*---------------------------------------------------------------------*/
ude:
	(cd runtime; $(MAKE) ude)
	(cd scheme2js; $(MAKE) ude)
	(cd src; $(MAKE) ude)

#*---------------------------------------------------------------------*/
#*    install                                                          */
#*---------------------------------------------------------------------*/
install: install-init
	(cd runtime && $(MAKE) install) && \
	(cd scheme2js && $(MAKE) install) && \
	(cd src && $(MAKE) install) && \
	(cd share && $(MAKE) install) && \
	(cd weblets && $(MAKE) install)
	(cd demos && $(MAKE) install)

install-init: $(DESTDIR)$(HOPFILDIR)
	cp $(BUILDLIBDIR)/hop.init $(DESTDIR)$(HOPFILDIR)/hop.init && \
        chmod $(BMASK) $(DESTDIR)$(HOPFILDIR)/hop.init;
	cp $(BUILDLIBDIR)/scheme2js.init $(DESTDIR)$(HOPFILDIR)/scheme2js.init && \
        chmod $(BMASK) $(DESTDIR)$(HOPFILDIR)/scheme2js.init;

$(DESTDIR)$(HOPFILDIR):
	mkdir -p $(DESTDIR)$(HOPFILDIR)

#*---------------------------------------------------------------------*/
#*    uninstall                                                        */
#*---------------------------------------------------------------------*/
uninstall:
	(cd src; $(MAKE) uninstall)
	(cd runtime; $(MAKE) uninstall)
	(cd scheme2js; $(MAKE) uninstall)
	(cd demos; $(MAKE) uninstall)
	/bin/rm -rf $(HOPFILDIR)

#*---------------------------------------------------------------------*/
#*    clean                                                            */
#*---------------------------------------------------------------------*/
clean:
	(cd runtime; $(MAKE) clean)
	(cd scheme2js; $(MAKE) clean)
	(cd src; $(MAKE) clean)

devclean:
	(cd runtime; $(MAKE) devclean)
	(cd src; $(MAKE) devclean)

distclean: clean devclean
	/bin/rm -f etc/Makefile.hopconfig
	/bin/rm -f lib/hop.init
	/bin/rm -f lib/scheme2js.init

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
               min=`expr $$min + 1`; \
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

 
