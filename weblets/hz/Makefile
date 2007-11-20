#*=====================================================================*/
#*    serrano/prgm/project/hop/weblets/hz/Makefile                     */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Fri Jan 20 14:35:57 2006                          */
#*    Last change :  Mon Nov 19 11:24:24 2007 (serrano)                */
#*    Copyright   :  2006-07 Manuel Serrano                            */
#*    -------------------------------------------------------------    */
#*    The Makefile of the HZ weblet                                    */
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

SOURCES		= config.hop hz.hop repository.hop weblets.hop \
		  db.hop hz.hss install.hop sync.hop \
                  markup.hop package.hop weblet.hop

POPULATION	= $(SOURCES) \
                  Makefile \
                  etc/icons .afile etc/weblet.info \
                  etc/logo.png etc/logo-16x16.png \
                  etc/hz.wiki \
                  hz-install.png hz-install.js

.afile: $(SOURCES)
	$(AFILE) -suffix hop $(SOURCES) > .afile
