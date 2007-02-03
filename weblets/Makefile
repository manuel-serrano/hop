#*=====================================================================*/
#*    serrano/prgm/project/hop/weblets/Makefile                        */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Sat Dec 25 07:19:48 2004                          */
#*    Last change :  Fri Feb  2 15:29:32 2007 (serrano)                */
#*    Copyright   :  2004-07 Manuel Serrano                            */
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
POPDIRS	= hop password hz info webfilter doc home hopsh wizard

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
	mkdir -p $(HOPWEBLETSDIR) && \
	chmod a+rx $(HOPWEBLETSDIR) && \
	for p in $(POPDIRS); do \
           $(MAKE) install-weblet DIR=$$p || exit 1; \
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
        chmod a+rx,u+rwx $(HOPWEBLETSDIR)/$(DIR) && \
	for f in $(DIR)/*; do \
          if [ -d $$f ]; then \
            install -m a+rx,u+rwx -d $$f || exit 1 && \
            make install-weblet DIR=$$f || exit 1; \
          else \
            if [ -f $$f ]; then \
	      install -m a+r,u+r $$f $(HOPWEBLETSDIR)/$(DIR) || exit 1; \
            fi; \
          fi; \
        done; \
        if [ -f $(DIR)/.afile ]; then \
          install -m a+r,u+r $(DIR)/.afile $(HOPWEBLETSDIR)/$(DIR) || exit 1; \
        fi; \

