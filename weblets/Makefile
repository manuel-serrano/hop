#*=====================================================================*/
#*    serrano/prgm/project/hop/1.9.x/weblets/Makefile                  */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Sat Dec 25 07:19:48 2004                          */
#*    Last change :  Mon Aug 18 13:00:33 2008 (serrano)                */
#*    Copyright   :  2004-08 Manuel Serrano                            */
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
POPDIRS	= hop epassword hz info webfilter doc home hopsh wizard test \
          dashboard wiki trace shutdown weblets

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
	mkdir -p $(DESTDIR)$(HOPWEBLETSDIR)/$(DIR) && \
        chmod a+rx,u+rwx $(DESTDIR)$(HOPWEBLETSDIR)/$(DIR) && \
	for f in $(DIR)/*; do \
          if [ -d $$f ]; then \
            install -m a+rx,u+rwx -d $$f || exit 1 && \
            $(MAKE) install-weblet DIR=$$f || exit 1; \
          else \
            if [ -f $$f ]; then \
	      install -m a+r,u+r $$f $(DESTDIR)$(HOPWEBLETSDIR)/$(DIR) \
                      || exit 1; \
            fi; \
          fi; \
        done; \
        if [ -f $(DIR)/.afile ]; then \
          install -m a+r,u+r $(DIR)/.afile $(DESTDIR)$(HOPWEBLETSDIR)/$(DIR) \
                  || exit 1; \
        fi; \

