#*=====================================================================*/
#*    serrano/prgm/project/hop/weblets/home/Makefile                   */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Fri Jan 20 14:35:57 2006                          */
#*    Last change :  Wed May 10 16:29:51 2006 (serrano)                */
#*    Copyright   :  2006 Manuel Serrano                               */
#*    -------------------------------------------------------------    */
#*    The Makefile of the HZ weblet                                    */
#*=====================================================================*/

#*---------------------------------------------------------------------*/
#*    Configuration                                                    */
#*---------------------------------------------------------------------*/
include ../../etc/Makefile.hopconfig
include ../../etc/Makefile.version

HOP_POP         = home.hop config.hop widgets.hop demo.hop
POPULATION	= Makefile doc.info .afile home.hss $(HOP_POP)

build: .afile

.afile: $(HOP_POP)
	afile -suffix hop $(POPULATION) > .afile
