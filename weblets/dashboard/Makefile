#*=====================================================================*/
#*    serrano/prgm/project/hop/1.9.x/weblets/dashboard/Makefile        */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Fri Jan 20 14:35:57 2006                          */
#*    Last change :  Tue Jan 22 09:23:23 2008 (serrano)                */
#*    Copyright   :  2006-08 Manuel Serrano                            */
#*    -------------------------------------------------------------    */
#*    The Makefile of the dashboard weblet                             */
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
                  dashboard-applet.hss etc/dashboard-rfc.png \
                  etc/logo.png

build: .afile

.afile: $(POPULATION)
	$(AFILE) -suffix hop $(POPULATION) -o .afile

clean:
