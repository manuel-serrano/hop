#*=====================================================================*/
#*    serrano/prgm/project/hop/weblets/doc/Makefile                    */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Fri Jan 20 14:35:57 2006                          */
#*    Last change :  Sat May 20 10:02:52 2006 (serrano)                */
#*    Copyright   :  2006 Manuel Serrano                               */
#*    -------------------------------------------------------------    */
#*    The Makefile of the HZ weblet                                    */
#*=====================================================================*/

#*---------------------------------------------------------------------*/
#*    Configuration                                                    */
#*---------------------------------------------------------------------*/
include ../../etc/Makefile.hopconfig
include ../../etc/Makefile.version

HOP_POP         = doc.hop config.hop \
                  api.hop tutorials.hop libraries.hop \
                  search.hop widgets.hop wiki.hop services.hop
POPULATION	= Makefile doc.info .afile doc doc.hss \
                  examples api wiki tutorials etc/logo.png $(HOP_POP)

build: .afile

.afile: $(HOP_POP)
	$(AFILE) -suffix hop $(POPULATION) > .afile
