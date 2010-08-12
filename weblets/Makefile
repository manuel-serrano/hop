#*=====================================================================*/
#*    serrano/prgm/project/hop/2.2.x/weblets/Makefile                  */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Sat Dec 25 07:19:48 2004                          */
#*    Last change :  Thu Aug 12 12:11:22 2010 (serrano)                */
#*    Copyright   :  2004-10 Manuel Serrano                            */
#*    -------------------------------------------------------------    */
#*    The Makefile for the HOP weblets directory                       */
#*=====================================================================*/

#*---------------------------------------------------------------------*/
#*    Configuration                                                    */
#*---------------------------------------------------------------------*/
-include ../etc/Makefile.hopconfig
-include ../etc/Makefile.version

#*---------------------------------------------------------------------*/
#*    POPULATION                                                       */
#*---------------------------------------------------------------------*/
POPULATION	= Makefile

#*---------------------------------------------------------------------*/
#*    weblets                                                          */
#*---------------------------------------------------------------------*/
POPDIRS	= hop epassword hz info webfilter doc home hopsh wizard test \
          dashboard color wiki trace shutdown weblets hzbuilder

#*---------------------------------------------------------------------*/
#*    build                                                            */
#*---------------------------------------------------------------------*/
build:
	for p in $(POPDIRS); do \
          ($(MAKE) -C $$p build) || exit 1;\
        done

#*---------------------------------------------------------------------*/
#*    clean                                                            */
#*---------------------------------------------------------------------*/
clean:
	for p in $(POPDIRS); do \
          ($(MAKE) -C $$p clean) || exit 1;\
        done

#*---------------------------------------------------------------------*/
#*    install                                                          */
#*---------------------------------------------------------------------*/
install:
	for p in $(POPDIRS); do \
           $(MAKE) install-weblet DIR=$$p || exit 1; \
        done

#*---------------------------------------------------------------------*/
#*    uninstall                                                        */
#*---------------------------------------------------------------------*/
uninstall:
	/bin/rm -f $(DESTDIR)$(SHAREDIR)/popdirs

#*---------------------------------------------------------------------*/
#*    install-weblet                                                   */
#*---------------------------------------------------------------------*/
install-weblet:
	if [ ! -d $(DESTDIR)$(HOPWEBLETSDIR)/$(DIR) ]; then \
	   mkdir -p $(DESTDIR)$(HOPWEBLETSDIR)/$(DIR) && \
              chmod $(MODDIR) $(DESTDIR)$(HOPWEBLETSDIR)/$(DIR); \
        fi
	for f in $(DIR)/*; do \
          if [ ! -d $$f ]; then \
            mkdir -p $$f chmod $(MODDIR) $$f || exit 1; \
          fi; \
          $(INSTALL) $$f $(DESTDIR)$(HOPWEBLETSDIR)/$$f && \
          chmod $(MODDIR) $(DESTDIR)$(HOPWEBLETSDIR)/$$f || exit 1; \
        done && \
        if [ -f $(DIR)/.afile ]; then \
          $(INSTALL) $(DIR)/.afile $(DESTDIR)$(HOPWEBLETSDIR)/$(DIR) && \
          chmod $(MODFILE) $(DESTDIR)$(HOPWEBLETSDIR)/$(DIR)/.afile || exit 1; \
        fi;

