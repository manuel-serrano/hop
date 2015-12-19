#*=====================================================================*/
#*    serrano/prgm/project/hop/3.0.x/etc/Makefile                      */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Fri Jan 20 13:46:40 2006                          */
#*    Last change :  Tue Dec 15 07:42:51 2015 (serrano)                */
#*    Copyright   :  2006-15 Manuel Serrano                            */
#*    -------------------------------------------------------------    */
#*    ETC Makefile                                                     */
#*=====================================================================*/

#*---------------------------------------------------------------------*/
#*    Configuration                                                    */
#*---------------------------------------------------------------------*/
-include ../etc/Makefile.hopconfig
-include ../etc/Makefile.version

#*---------------------------------------------------------------------*/
#*    Population                                                       */
#*---------------------------------------------------------------------*/
POPULATION	= Makefile Makefile.weblet Makefile.library \
                  Makefile.mercurial Makefile.git Makefile.version \
		  Makefile.hopconfig.in \
                  configure.scm.in libraries.sch.in \
                  configure_noarch.sch.in configure_macosx.sch.in \
                  configure_android.sch.in \
                  hopscript.sch.in nodejs.sch.in \
                  hoprc.hop.in hopshrc.hop.in hophzrc.hop.in \
		  hop.init.in scheme2js.init.in hopscheme.init.in \
                  js2scheme.init.in hopscript.init.in nodejs.init.in \
                  hopdroid.init.in \
                  hop.man.in hopsh.man.in hopreplay.man.in \
                  hopc.sh.in hopwidget.init.in hglog.scm hopjs.el.in \
                  node_crypto_groups.h node_crypto_groups.c \
                  mkserializer.hop hop.sh.in hopc.sh.in \
                  hop-autoconf.js.in hop-boot.js.in

install:
	cp Makefile.hopconfig $(DESTDIR)$(HOPLIBDIR)/hop/$(HOPRELEASE)/Makefile.hopconfig
	chmod $(MODFILE) $(DESTDIR)$(HOPLIBDIR)/hop/$(HOPRELEASE)/Makefile.hopconfig
	cp hoprc.hop $(DESTDIR)$(HOPETCDIR)/hoprc.hop
	chmod $(MODFILE) $(DESTDIR)$(HOPETCDIR)/hoprc.hop
	cp hopshrc.hop $(DESTDIR)$(HOPETCDIR)/hopshrc.hop
	chmod $(MODFILE) $(DESTDIR)$(HOPETCDIR)/hopshrc.hop
	if [ ! -d $(DESTDIR)$(HOPMANDIR) ]; then \
	  $(MAKE) mkdir DIR=$(DESTDIR)$(HOPMANDIR); \
        fi
	for p in hop hopsh hopreplay; do \
          cp $$p.man $(DESTDIR)$(HOPMANDIR)/$$p.1 && \
          chmod $(MODFILE) $(DESTDIR)$(HOPMANDIR)/$$p.1; \
        done
	if [ ! -d $(DESTDIR)$(HOPSHAREDIR)/site-lisp ]; then \
          if [ -f $(DESTDIR)$(HOPSHAREDIR)/site-lisp ]; then \
            $(RM) $(DESTDIR)$(HOPSHAREDIR)/site-lisp; \
          fi; \
	  $(MAKE) mkdir DIR=$(DESTDIR)$(HOPSHAREDIR)/site-lisp; \
	  chmod $(MODDIR) $(DESTDIR)$(HOPSHAREDIR)/site-lisp; \
	fi
	cp hopjs.el $(DESTDIR)$(HOPSHAREDIR)/site-lisp/

uninstall:
	$(RM) -f $(DESTDIR)$(HOPLIBDIR)/hop/$(HOPRELEASE)/Makefile.hopconfig
	$(RM) -f $(DESTDIR)$(HOPETCDIR)/hoprc.hop
	$(RM) -f $(DESTDIR)$(HOPMANDIR)/hop.1
	$(RM) -f $(DESTDIR)$(HOPMANDIR)/hopsh.1
	$(RM) -f $(DESTDIR)$(HOPMANDIR)/hopreplay.1

clean:


devclean: clean
	$(RM) -f hopshrc.hop
	$(RM) -f hop.man hopreplay.man hopsh.man

