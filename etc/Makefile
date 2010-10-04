#*=====================================================================*/
#*    serrano/prgm/project/hop/2.2.x/etc/Makefile                      */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Fri Jan 20 13:46:40 2006                          */
#*    Last change :  Mon Oct  4 07:50:30 2010 (serrano)                */
#*    Copyright   :  2006-10 Manuel Serrano                            */
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
POPULATION	= Makefile Makefile.weblet \
                  Makefile.mercurial Makefile.version Makefile.library \
		  Makefile.hopconfig.in \
                  configure.scm.in library.sch.in \
                  configure-noarch.sch.in configure-macosx.sch.in \
                  configure-android.sch.in \
                  hoprc.hop.in hopshrc.hop.in \
		  hop.init.in scheme2js.init.in hopscheme.init.in \
                  hop.man.in hopsh.man.in hopreplay.man.in \
                  hopc.sh.in hopwidget.init.in

install:
	cp Makefile.hopconfig $(DESTDIR)$(HOPLIBDIR)/hop/$(HOPRELEASE)/Makefile.hopconfig
	chmod $(MODFILE) $(DESTDIR)$(HOPLIBDIR)/hop/$(HOPRELEASE)/Makefile.hopconfig
	cp hoprc.hop $(DESTDIR)$(HOPETCDIR)/hoprc.hop
	chmod $(MODFILE) $(DESTDIR)$(HOPETCDIR)/hoprc.hop
	cp hopshrc.hop $(DESTDIR)$(HOPETCDIR)/hopshrc.hop
	chmod $(MODFILE) $(DESTDIR)$(HOPETCDIR)/hopshrc.hop
	if [ ! -d $(DESTDIR)$(HOPMANDIR) ]; then \
	  mkdir -p $(DESTDIR)$(HOPMANDIR) && \
            chmod $(MODDIR) $(DESTDIR)$(HOPMANDIR); \
        fi
	for p in hop hopsh hopreplay; do \
          cp $$p.man $(DESTDIR)$(HOPMANDIR)/$$p.1 && \
          chmod $(MODFILE) $(DESTDIR)$(HOPMANDIR)/$$p.1; \
        done

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

