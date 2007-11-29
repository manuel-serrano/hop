#*=====================================================================*/
#*    serrano/prgm/project/hop/weblets/weblets/Makefile                */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Fri Jan 20 14:35:57 2006                          */
#*    Last change :  Thu Nov 29 07:30:08 2007 (serrano)                */
#*    Copyright   :  2006-07 Manuel Serrano                            */
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

SOURCES		= weblets.hop package.hop config.hop

POPULATION	= $(SOURCES) \
                  Makefile \
                  weblets.hss config.hop.in \
                  LICENSE \
                  etc/logo.png etc/favicon.png \
                  etc/weblet.info etc/weblets.wiki .afile

.afile: $(SOURCES)
	$(AFILE) -suffix hop $(SOURCES) > .afile
