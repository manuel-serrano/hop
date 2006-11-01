#*=====================================================================*/
#*    serrano/prgm/project/hop/weblets/doc/Makefile                    */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Fri Jan 20 14:35:57 2006                          */
#*    Last change :  Wed Nov  1 16:18:01 2006 (serrano)                */
#*    Copyright   :  2006 Manuel Serrano                               */
#*    -------------------------------------------------------------    */
#*    The Makefile of the DOC weblet                                   */
#*=====================================================================*/

#*---------------------------------------------------------------------*/
#*    Configuration                                                    */
#*---------------------------------------------------------------------*/
include ../../etc/Makefile.hopconfig
include ../../etc/Makefile.version

HOP_POP         = doc.hop config.hop \
                  api.hop tutorials.hop libraries.hop weblets.hop \
                  search.hop widgets.hop wiki.hop services.hop index.hop
POPULATION	= Makefile .afile doc.hss \
                  examples api core wiki tutorials etc/weblet.info \
                  etc/logo.png etc/doc.wiki \
                  $(HOP_POP)

build: .afile

.afile: $(HOP_POP)
	$(AFILE) -suffix hop $(POPULATION) > .afile
