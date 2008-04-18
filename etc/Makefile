#*=====================================================================*/
#*    serrano/prgm/project/hop/1.9.x/etc/Makefile                      */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Fri Jan 20 13:46:40 2006                          */
#*    Last change :  Fri Apr 18 11:41:16 2008 (serrano)                */
#*    Copyright   :  2006-08 Manuel Serrano                            */
#*    -------------------------------------------------------------    */
#*    ETC Makefile                                                     */
#*=====================================================================*/

#*---------------------------------------------------------------------*/
#*    Configuration                                                    */
#*---------------------------------------------------------------------*/
include ../etc/Makefile.hopconfig
include ../etc/Makefile.version

#*---------------------------------------------------------------------*/
#*    Population                                                       */
#*---------------------------------------------------------------------*/
POPULATION	= Makefile Makefile.contrib Makefile.weblet \
                  Makefile.mercurial Makefile.version Makefile.library \
		  Makefile.hopconfig.in configure.scm.in hoprc.hop.in \
		  hop.init.in scheme2js.init.in hopscheme.init.in \
                  hopwiki.init.in

install:
	cp Makefile.hopconfig $(DESTDIR)$(HOPLIBDIR)/hop/$(HOPRELEASE)/Makefile.hopconfig
	chmod $(BMASK) $(DESTDIR)$(HOPLIBDIR)/hop/$(HOPRELEASE)/Makefile.hopconfig
	cp hoprc.hop $(DESTDIR)$(HOPETCDIR)/hoprc.hop
	chmod $(BMASK) $(DESTDIR)$(HOPETCDIR)/hoprc.hop

uninstall:
	$(RM) -f $(DESTDIR)$(HOPLIBDIR)/hop/$(HOPRELEASE)/Makefile.hopconfig
	$(RM) -f $(DESTDIR)$(HOPETCDIR)/hoprc.hop

