#*=====================================================================*/
#*    serrano/prgm/project/hop/weblets/hz/Makefile                     */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Fri Jan 20 14:35:57 2006                          */
#*    Last change :  Thu Nov 29 07:29:11 2007 (serrano)                */
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

SOURCES		= config.hop hz.hop \
		  db.hop hz.hss install.hop sync.hop \
                  markup.hop package.hop weblet.hop \
                  authenticate.hop

POPULATION	= $(SOURCES) \
                  Makefile \
                  etc/icons .afile etc/weblet.info \
                  etc/logo.png etc/favicon.png \
                  etc/hz.wiki \
                  etc/hz-install.png etc/hz-run.png hz-install.js

.afile: $(SOURCES)
	$(AFILE) -suffix hop $(SOURCES) > .afile
