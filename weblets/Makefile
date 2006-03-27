#*=====================================================================*/
#*    serrano/prgm/project/hop/weblets/Makefile                        */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Sat Dec 25 07:19:48 2004                          */
#*    Last change :  Mon Mar 27 18:44:26 2006 (serrano)                */
#*    Copyright   :  2004-06 Manuel Serrano                            */
#*    -------------------------------------------------------------    */
#*    The Makefile for the HOP weblets directory                       */
#*=====================================================================*/

#*---------------------------------------------------------------------*/
#*    Configuration                                                    */
#*---------------------------------------------------------------------*/
include ../etc/Makefile.hopconfig
include ../etc/Makefile.version

#*---------------------------------------------------------------------*/
#*    POPULATION                                                       */
#*---------------------------------------------------------------------*/
POPULATION	= Makefile

#*---------------------------------------------------------------------*/
#*    weblets                                                          */
#*---------------------------------------------------------------------*/
POPDIRS	= password hz info jobs dired webconf webfilter demos

#*---------------------------------------------------------------------*/
#*    install                                                          */
#*---------------------------------------------------------------------*/
install:
	mkdir -p $(HOPWEBLETSDIR) && \
	chmod a+rx $(HOPWEBLETSDIR) && \
	for p in $(POPDIRS); do \
           ($(MAKE) install-weblet DIR=$$p || exit -1) \
        done

#*---------------------------------------------------------------------*/
#*    uninstall                                                        */
#*---------------------------------------------------------------------*/
uninstall:
	/bin/rm -f $(SHAREDIR)/popdirs

#*---------------------------------------------------------------------*/
#*    install-weblet                                                   */
#*---------------------------------------------------------------------*/
install-weblet:
	mkdir -p $(HOPWEBLETSDIR)/$(DIR) && \
        chmod a+rx $(HOPWEBLETSDIR)/$(DIR) && \
	for f in $(DIR)/*; do \
          if [ -d $$f ]; then \
            install -m a+rx -d $(HOPWEBLETSDIR)/$(DIR); \
            make install-weblet DIR=$$f; \
          else \
	    install -m a+r $$f $(HOPWEBLETSDIR)/$(DIR); \
          fi; \
        done; \
        if [ -f $(DIR)/.afile ]; then \
          install -m a+r $(DIR)/.afile $(HOPWEBLETSDIR)/$(DIR); \
        fi; \

