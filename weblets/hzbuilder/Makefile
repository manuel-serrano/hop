#*=====================================================================*/
#*    serrano/prgm/project/hop/1.9.x/weblets/hzbuilder/Makefile        */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Fri Jan 20 14:35:57 2006                          */
#*    Last change :  Thu Sep 25 10:22:31 2008 (serrano)                */
#*    Copyright   :  2007-08 Manuel Serrano                            */
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
POPULATION=hzbuilder.hop generate.hop test.hop hzbuilder.hss \
  etc/weblet.info.in etc/hzbuilder.wiki.in config.hop.in \
  etc/logo.png etc/favicon.png etc/doc.png etc/configure.png \
  etc/logo-default.png etc/favicon-default.png \
  etc/Makefile.hz etc/default.hss \
  Makefile LICENSE

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
include ../../etc/Makefile.hopconfig
include ../../etc/Makefile.version
include ../../etc/Makefile.weblet
