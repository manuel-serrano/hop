#*=====================================================================*/
#*    serrano/prgm/project/hop/weblets/hz/Makefile                     */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Fri Jan 20 14:35:57 2006                          */
#*    Last change :  Mon May 22 09:31:12 2006 (serrano)                */
#*    Copyright   :  2006 Manuel Serrano                               */
#*    -------------------------------------------------------------    */
#*    The Makefile of the HZ weblet                                    */
#*=====================================================================*/

#*---------------------------------------------------------------------*/
#*    Configuration                                                    */
#*---------------------------------------------------------------------*/
include ../../etc/Makefile.hopconfig
include ../../etc/Makefile.version

POPULATION	= Makefile \
                  icons config.hop hz.hop repository.hop weblets.hop \
		  authenticate.hop db.hop hz.hss install.hop sync.hop \
                  widgets.hop .afile etc/hz.info etc/logo.png

build: .afile

.afile:
	$(AFILE) -suffix hop $(POPULATION) > .afile
