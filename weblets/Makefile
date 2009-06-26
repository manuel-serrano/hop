#*=====================================================================*/
#*    serrano/prgm/project/hop/2.0.x/weblets/Makefile                  */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Sat Dec 25 07:19:48 2004                          */
#*    Last change :  Fri Jun 26 13:31:04 2009 (serrano)                */
#*    Copyright   :  2004-09 Manuel Serrano                            */
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
	if [ "$(HOPVERSION) " != " " ]; then \
	  for p in $(POPDIRS); do \
            ($(MAKE) -C $$p clean) || exit 1;\
          done \
        fi

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
            mkdir -p $$f || exit 1; \
            chmod a+rx,u+rwx $$f || exit 1; \
            $(MAKE) install-weblet DIR=$$f || exit 1; \
          else \
            if [ -f $$f ]; then \
	      $(INSTALL) $$f $(DESTDIR)$(HOPWEBLETSDIR)/$(DIR) || exit 1; \
            fi; \
          fi; \
        done; \
        if [ -f $(DIR)/.afile ]; then \
          $(INSTALL) $(DIR)/.afile $(DESTDIR)$(HOPWEBLETSDIR)/$(DIR) || exit 1; \
        fi; \

