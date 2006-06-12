#*=====================================================================*/
#*    serrano/prgm/project/hop/weblets/doc/Makefile                    */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Fri Jan 20 14:35:57 2006                          */
#*    Last change :  Mon Jun 12 11:50:23 2006 (eg)                     */
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
                  search.hop widgets.hop wiki.hop services.hop
POPULATION	= Makefile .afile doc.hss \
                  examples api core wiki tutorials etc/weblet-info etc/logo.png \
                  $(HOP_POP)

build: .afile

.afile: $(HOP_POP)
	$(AFILE) -suffix hop $(POPULATION) > .afile
