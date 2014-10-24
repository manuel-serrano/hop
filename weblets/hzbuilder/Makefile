#*=====================================================================*/
#*    serrano/prgm/project/hop/3.0.x/weblets/hzbuilder/Makefile        */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Fri Jan 20 14:35:57 2006                          */
#*    Last change :  Fri Oct 24 15:49:05 2014 (serrano)                */
#*    Copyright   :  2007-14 Manuel Serrano                            */
#*    -------------------------------------------------------------    */
#*    Weblet Makefile                                                  */
#*=====================================================================*/

#*---------------------------------------------------------------------*/
#*    CONFIG ...                                                       */
#*---------------------------------------------------------------------*/
HZ=hzbuilder
VERSION=1.0.0
MINHOP=$(HOPRELEASE) 
CATEGORY=hop
MINHOP=1.9.0
DATE=24 sep 2008

#*---------------------------------------------------------------------*/
#*    POPULATION                                                       */
#*---------------------------------------------------------------------*/
SOURCES= hzbuilder.hop generate.hop test.hop hzbuilder.hss config.hop.in
POPULATION= $(SOURCES) \
  etc/weblet.info.in etc/hzbuilder.wiki.in  \
  etc/logo.png etc/favicon.png etc/doc.png etc/configure.png \
  etc/skeleton/Makefile \
  etc/skeleton/default.hss etc/skeleton/default-hopscript.hss\
  etc/skeleton/main.hop etc/skeleton/config.hop.in \
  etc/skeleton/main.js etc/skeleton/config.js.in \
  etc/skeleton/doc.wiki.in etc/skeleton/weblet.info.in \
  etc/skeleton/logo.png etc/skeleton/favicon.png etc/skeleton/dashboard.png \
  etc/skeleton/01-homepage.wiki \
  Makefile

#*---------------------------------------------------------------------*/
#*    Targets                                                          */
#*---------------------------------------------------------------------*/
build: etc/weblet.info etc/$(HZ).wiki config.hop .afile 

clean:

distclean:
	$(RM) etc/weblet.info etc/$(HZ).wiki

#*---------------------------------------------------------------------*/
#*    Configuration                                                    */
#*---------------------------------------------------------------------*/
-include ../../etc/Makefile.hopconfig
-include ../../etc/Makefile.version
include ../../etc/Makefile.weblet
