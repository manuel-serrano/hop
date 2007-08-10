#*=====================================================================*/
#*    serrano/prgm/project/hop/weblets/dashboard/Makefile              */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Fri Jan 20 14:35:57 2006                          */
#*    Last change :  Mon Jul  9 08:00:15 2007 (serrano)                */
#*    Copyright   :  2006-07 Manuel Serrano                            */
#*    -------------------------------------------------------------    */
#*    The Makefile of the wizard weblet                                */
#*=====================================================================*/

#*---------------------------------------------------------------------*/
#*    Configuration                                                    */
#*---------------------------------------------------------------------*/
include ../../etc/Makefile.hopconfig
include ../../etc/Makefile.version

#*---------------------------------------------------------------------*/
#*    POPULATION                                                       */
#*---------------------------------------------------------------------*/
POPULATION	= Makefile dashboard.hss dashboard.hop \
                  etc/weblet.info etc/dashboard.wiki \
                  etc/logo.png

build: .afile

.afile: $(POPULATION)
	$(AFILE) -suffix hop $(POPULATION) > .afile

clean:
