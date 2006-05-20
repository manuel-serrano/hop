#*=====================================================================*/
#*    serrano/prgm/project/hop/weblets/hop/Makefile                    */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Fri Jan 20 14:35:57 2006                          */
#*    Last change :  Sat May 20 10:44:51 2006 (serrano)                */
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
POPULATION	= Makefile hop.info .afile hop.hss \
                  etc/logo.png etc/default.png $(HOP_POP) 

build: .afile

.afile: $(HOP_POP)
	$(AFILE) -suffix hop $(POPULATION) > .afile
