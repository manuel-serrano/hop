#*=====================================================================*/
#*    serrano/prgm/project/hop/2.0.x/etc/Makefile                      */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Fri Jan 20 13:46:40 2006                          */
#*    Last change :  Tue Jun 23 15:25:34 2009 (serrano)                */
#*    Copyright   :  2006-09 Manuel Serrano                            */
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
POPULATION	= Makefile Makefile.contrib Makefile.weblet \
                  Makefile.mercurial Makefile.version Makefile.library \
		  Makefile.hopconfig.in \
                  configure.scm.in \
                  configure-noarch.sch.in configure-macosx.sch.in \
                  hoprc.hop.in hopshrc.hop.in \
		  hop.init.in scheme2js.init.in hopscheme.init.in \
                  hopwiki.init.in hop.man.in hopsh.man.in hopreplay.man.in \
                  hopc.sh.in hopwidget.init.in

install:
	cp Makefile.hopconfig $(DESTDIR)$(HOPLIBDIR)/hop/$(HOPRELEASE)/Makefile.hopconfig
	chmod $(BMASK) $(DESTDIR)$(HOPLIBDIR)/hop/$(HOPRELEASE)/Makefile.hopconfig
	cp hoprc.hop $(DESTDIR)$(HOPETCDIR)/hoprc.hop
	chmod $(BMASK) $(DESTDIR)$(HOPETCDIR)/hoprc.hop
	cp hopshrc.hop $(DESTDIR)$(HOPETCDIR)/hopshrc.hop
	chmod $(BMASK) $(DESTDIR)$(HOPETCDIR)/hopshrc.hop
	mkdir -p $(DESTDIR)$(HOPMANDIR) && chmod $(BMASK) $(DESTDIR)$(HOPMANDIR)
	for p in hop hopsh hopreplay; do \
          cp $$p.man $(DESTDIR)$(HOPMANDIR)/$$p.1 && \
          chmod $(BMASK) $(DESTDIR)$(HOPMANDIR)/$$p.1; \
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

