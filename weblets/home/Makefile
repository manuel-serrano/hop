#*=====================================================================*/
#*    serrano/prgm/project/hop/weblets/home/Makefile                   */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Fri Jan 20 14:35:57 2006                          */
#*    Last change :  Sun May 14 17:34:41 2006 (serrano)                */
#*    Copyright   :  2006 Manuel Serrano                               */
#*    -------------------------------------------------------------    */
#*    The Makefile of the HZ weblet                                    */
#*=====================================================================*/

#*---------------------------------------------------------------------*/
#*    Configuration                                                    */
#*---------------------------------------------------------------------*/
include ../../etc/Makefile.hopconfig
include ../../etc/Makefile.version

HOP_POP         = home.hop config.hop widgets.hop demo.hop \
                  presentation.wiki docs.wiki license.wiki
POPULATION	= Makefile home.info .afile home.hss $(HOP_POP) \
                  demos icons articles

build: .afile

.afile: $(HOP_POP)
	afile -suffix hop $(POPULATION) demos/*/*.hop > .afile
