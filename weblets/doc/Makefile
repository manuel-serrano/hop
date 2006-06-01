#*=====================================================================*/
#*    serrano/prgm/project/hop/weblets/doc/Makefile                    */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Fri Jan 20 14:35:57 2006                          */
#*    Last change :  Mon May 29 14:13:30 2006 (serrano)                */
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
POPULATION	= Makefile .afile doc.hss \
                  examples api core wiki tutorials etc/doc.info etc/logo.png \
                  $(HOP_POP)

build: .afile

.afile: $(HOP_POP)
	$(AFILE) -suffix hop $(POPULATION) > .afile
