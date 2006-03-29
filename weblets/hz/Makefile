#*=====================================================================*/
#*    serrano/prgm/project/hop/weblets/hz/Makefile                     */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Fri Jan 20 14:35:57 2006                          */
#*    Last change :  Wed Mar 29 11:49:39 2006 (serrano)                */
#*    Copyright   :  2006 Manuel Serrano                               */
#*    -------------------------------------------------------------    */
#*    The Makefile of the HZ weblet                                    */
#*=====================================================================*/

#*---------------------------------------------------------------------*/
#*    Configuration                                                    */
#*---------------------------------------------------------------------*/
include ../../etc/Makefile.hopconfig
include ../../etc/Makefile.version

POPULATION	= Makefile hz.info \
                  icons config.hop hz.hop repository.hop weblets.hop \
		  authenticate.hop db.hop hz.hss install.hop sync.hop \
                  widgets.hop .afile

build: .afile

.afile:
	afile -suffix hop $(POPULATION) > .afile

clean:
	/bin/rm -f .afile
