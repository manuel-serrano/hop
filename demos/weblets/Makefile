#*=====================================================================*/
#*    serrano/prgm/project/hop/demos/weblets/Makefile                  */
#*    -------------------------------------------------------------    */
#*    Author      :  Erick Gallesio                                    */
#*    Creation    :  Wed Mar  1 14:25:06 2006                          */
#*    Last change :  Thu Mar  2 21:20:37 2006 (eg)                     */
#*    -------------------------------------------------------------    */
#*    The Makefile for the HOP demos/weblets directory                 */
#*=====================================================================*/

#*---------------------------------------------------------------------*/
#*    Configuration                                                    */
#*---------------------------------------------------------------------*/
include ../../etc/Makefile.hopconfig
include ../../etc/Makefile.version

DEMOSDIR=$(HOPSHAREDIR)/demos/weblets

#*---------------------------------------------------------------------*/
#*    SUBDIRS                                                          */
#*---------------------------------------------------------------------*/
SUBDIRS =  notepad paned slider tabslider tree

#*---------------------------------------------------------------------*/
#*    install                                                          */
#*---------------------------------------------------------------------*/
install:
	for d in $(SUBDIRS) ;do \
	  mkdir -p $(DEMOSDIR)/$$d; \
	  if [ -f $$d/Makefile ] ;then \
	    (cd $$d; $(MAKE) install); \
	  else \
	    install -m a+r $$d/* $(DEMOSDIR)/$$d ;\
	  fi; \
	done
