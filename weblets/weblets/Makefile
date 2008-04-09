#*=====================================================================*/
#*    serrano/prgm/project/hop/1.9.x/weblets/weblets/Makefile          */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Fri Jan 20 14:35:57 2006                          */
#*    Last change :  Tue Jan 22 09:25:26 2008 (serrano)                */
#*    Copyright   :  2006-08 Manuel Serrano                            */
#*    -------------------------------------------------------------    */
#*    Weblet Makefile                                                  */
#*=====================================================================*/

#*---------------------------------------------------------------------*/
#*    build                                                            */
#*---------------------------------------------------------------------*/
build: .afile

#*---------------------------------------------------------------------*/
#*    Configuration                                                    */
#*---------------------------------------------------------------------*/
include ../../etc/Makefile.hopconfig
include ../../etc/Makefile.version

SOURCES		= weblets.hop package.hop config.hop markup.hop \
                  homepage.hop fontifier.hop

POPULATION	= $(SOURCES) \
                  Makefile \
                  weblets.hss weblets-homepage.hss config.hop \
                  LICENSE \
                  etc/logo.png etc/favicon.png \
                  etc/weblet.info etc/weblets.wiki .afile

.afile: $(SOURCES)
	$(AFILE) -suffix hop $(SOURCES) -o .afile
