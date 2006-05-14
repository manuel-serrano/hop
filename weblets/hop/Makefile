#*=====================================================================*/
#*    serrano/prgm/project/hop/weblets/hop/Makefile                    */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Fri Jan 20 14:35:57 2006                          */
#*    Last change :  Sat May 13 16:40:06 2006 (serrano)                */
#*    Copyright   :  2006 Manuel Serrano                               */
#*    -------------------------------------------------------------    */
#*    The Makefile of the HZ weblet                                    */
#*=====================================================================*/

#*---------------------------------------------------------------------*/
#*    Configuration                                                    */
#*---------------------------------------------------------------------*/
include ../../etc/Makefile.hopconfig
include ../../etc/Makefile.version

HOP_POP         = hop.hop config.hop
POPULATION	= Makefile hop.info .afile hop.hss $(HOP_POP)

build: .afile

.afile: $(HOP_POP)
	afile -suffix hop $(POPULATION) > .afile
