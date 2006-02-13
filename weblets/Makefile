#*=====================================================================*/
#*    serrano/prgm/project/hop/weblets/Makefile                        */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Sat Dec 25 07:19:48 2004                          */
#*    Last change :  Sun Feb 12 14:31:56 2006 (serrano)                */
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
POPULATION	= Makefile BLACKLIST

#*---------------------------------------------------------------------*/
#*    weblets                                                          */
#*---------------------------------------------------------------------*/
POPDIRS	= password info jobs dired bbdb webconf webfilter

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
	    $(MAKE) install-weblet DIR=$$f; \
          else \
	    install -m a+r $$f $(HOPWEBLETSDIR)/$(DIR); \
          fi; \
        done



