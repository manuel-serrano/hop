#*=====================================================================*/
#*    serrano/prgm/project/hop/1.9.x/weblets/shutdown/Makefile         */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Fri Jan 20 14:35:57 2006                          */
#*    Last change :  Tue Jan 22 09:25:08 2008 (serrano)                */
#*    Copyright   :  2006-08 Manuel Serrano                            */
#*    -------------------------------------------------------------    */
#*    The Makefile of the shutdown weblet                              */
#*=====================================================================*/

#*---------------------------------------------------------------------*/
#*    Configuration                                                    */
#*---------------------------------------------------------------------*/
include ../../etc/Makefile.hopconfig
include ../../etc/Makefile.version

#*---------------------------------------------------------------------*/
#*    POPULATION                                                       */
#*---------------------------------------------------------------------*/
POPULATION	= Makefile shutdown.hop shutdown.hss \
                  etc/weblet.info etc/shutdown.wiki \
                  etc/logo.png

build: .afile

.afile: $(POPULATION)
	$(AFILE) -suffix hop $(POPULATION) -o .afile

clean:
