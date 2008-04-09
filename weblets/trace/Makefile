#*=====================================================================*/
#*    serrano/prgm/project/hop/1.9.x/weblets/trace/Makefile            */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Fri Jan 20 14:35:57 2006                          */
#*    Last change :  Tue Jan 22 09:25:16 2008 (serrano)                */
#*    Copyright   :  2006-08 Manuel Serrano                            */
#*    -------------------------------------------------------------    */
#*    The Makefile of the trace weblet                                 */
#*=====================================================================*/

#*---------------------------------------------------------------------*/
#*    Configuration                                                    */
#*---------------------------------------------------------------------*/
include ../../etc/Makefile.hopconfig
include ../../etc/Makefile.version

#*---------------------------------------------------------------------*/
#*    POPULATION                                                       */
#*---------------------------------------------------------------------*/
POPULATION	= Makefile trace.hop \
                  etc/weblet.info etc/trace.wiki \
                  etc/logo.png

build: .afile

.afile: $(POPULATION)
	$(AFILE) -suffix hop $(POPULATION) -o .afile

clean:
