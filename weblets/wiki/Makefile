#*=====================================================================*/
#*    serrano/prgm/project/hop/weblets/wiki/Makefile                   */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Fri Jan 20 14:35:57 2006                          */
#*    Last change :  Thu Nov 29 07:29:39 2007 (serrano)                */
#*    Copyright   :  2006-07 Manuel Serrano                            */
#*    -------------------------------------------------------------    */
#*    The Makefile of the wiki weblet                                  */
#*=====================================================================*/

#*---------------------------------------------------------------------*/
#*    Configuration                                                    */
#*---------------------------------------------------------------------*/
include ../../etc/Makefile.hopconfig
include ../../etc/Makefile.version

#*---------------------------------------------------------------------*/
#*    POPULATION                                                       */
#*---------------------------------------------------------------------*/
POPULATION	= Makefile wiki.hss wiki.hop \
                  etc/weblet.info etc/wiki.wiki \
                  etc/logo.png etc/favicon.png

build: .afile

.afile: $(POPULATION)
	$(AFILE) -suffix hop $(POPULATION) > .afile

clean:
