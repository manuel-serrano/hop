#*=====================================================================*/
#*    serrano/prgm/project/hop/1.9.x/weblets/hop/Makefile              */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Fri Jan 20 14:35:57 2006                          */
#*    Last change :  Tue Jan 22 09:24:49 2008 (serrano)                */
#*    Copyright   :  2006-08 Manuel Serrano                            */
#*    -------------------------------------------------------------    */
#*    The Makefile of the HZ weblet                                    */
#*=====================================================================*/

#*---------------------------------------------------------------------*/
#*    Configuration                                                    */
#*---------------------------------------------------------------------*/
include ../../etc/Makefile.hopconfig
include ../../etc/Makefile.version

HOP_POP         = hop.hop config.hop
POPULATION	= Makefile .afile hop.hss \
                  etc/weblet.info etc/logo.png etc/default.png $(HOP_POP) 

build: .afile

.afile: $(HOP_POP)
	$(AFILE) -suffix hop $(POPULATION) -o .afile
