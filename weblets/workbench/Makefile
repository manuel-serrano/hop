#*=====================================================================*/
#*    serrano/prgm/project/hop/2.2.x/weblets/workbench/Makefile        */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Fri Jan 20 14:35:57 2006                          */
#*    Last change :  Sun Nov 21 09:02:14 2010 (serrano)                */
#*    Copyright   :  2006-10 Manuel Serrano                            */
#*    -------------------------------------------------------------    */
#*    The Makefile of the workbench weblet                             */
#*=====================================================================*/

#*---------------------------------------------------------------------*/
#*    CONFIG ...                                                       */
#*---------------------------------------------------------------------*/
TITLE = workbench
VERSION = 1.0.0
MINHOP = $(HOPRELEASE) 
MAXHOP =
CATEGORY = hop
DATE = 20 nov 2010

#*---------------------------------------------------------------------*/
#*    POPULATION                                                       */
#*---------------------------------------------------------------------*/
SOURCES		= workbench.hop workbench.hss workbench.scm config.scm
POPULATION	= Makefile $(SOURCES) \
		  etc/weblet.info.in etc/workbench.wiki.in \
                  etc/logo.png etc/favicon.png \
                  etc/clear.png etc/log1.png etc/log2.png
                  
build: etc/weblet.info etc/$(TITLE).wiki

clean:

distclean:
	$(RM) etc/weblet.info etc/$(TITLE).wiki

#*---------------------------------------------------------------------*/
#*    Configuration                                                    */
#*---------------------------------------------------------------------*/
-include ../../etc/Makefile.hopconfig
-include ../../etc/Makefile.version
include ../../etc/Makefile.weblet

