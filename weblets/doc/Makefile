#*=====================================================================*/
#*    serrano/prgm/project/hop/private/doc/Makefile                    */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Fri Jan 20 14:35:57 2006                          */
#*    Last change :  Mon Apr 24 15:01:58 2006 (serrano)                */
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
                  api.hop wikisyntax.hop \
                  search.hop widgets.hop wiki.hop services.hop
POPULATION	= Makefile doc.info .afile doc doc.hss $(HOP_POP)

build: .afile

.afile: $(HOP_POP)
	afile -suffix hop $(POPULATION) > .afile
