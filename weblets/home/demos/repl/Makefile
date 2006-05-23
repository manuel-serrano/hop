#*=====================================================================*/
#*    serrano/prgm/project/hop/weblets/home/demos/repl/Makefile        */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Fri Mar  3 10:30:36 2006                          */
#*    Last change :  Wed May 17 15:49:08 2006 (serrano)                */
#*    Copyright   :  2006 Manuel Serrano                               */
#*    -------------------------------------------------------------    */
#*    Makefile of a HOP demo                                           */
#*=====================================================================*/

#*---------------------------------------------------------------------*/
#*    Configuration                                                    */
#*---------------------------------------------------------------------*/
include ../../etc/Makefile.hopconfig
include ../../etc/Makefile.version

FILES      = repl.hop
POPULATION = Makefile $(FILES)
DESTDIR    = $(HOPDEMOSDIR)/$(DEMO)

#*---------------------------------------------------------------------*/
#*    install                                                          */
#*---------------------------------------------------------------------*/
install:
	mkdir -p $(DESTDIR)
	install -m a+r $(POPULATION) $(DESTDIR)
