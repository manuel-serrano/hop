#*=====================================================================*/
#*    serrano/prgm/project/hop/demos/widgets/foldlist/Makefile         */
#*    -------------------------------------------------------------    */
#*    Author      :  Erick Gallesio                                    */
#*    Creation    :  Tue Mar  7 14:36:14 2006                          */
#*    Last change :  Fri Mar  3 10:30:36 2006                          */
#*    -------------------------------------------------------------    */
#*    Makefile of a HOP demo                                           */
#*=====================================================================*/

#*---------------------------------------------------------------------*/
#*    Configuration                                                    */
#*---------------------------------------------------------------------*/
include ../../../etc/Makefile.hopconfig
include ../../../etc/Makefile.version

FILES      = foldlist.hop demofl.css
POPULATION = Makefile $(FILES)
DESTDIR    = $(HOPDEMOSDIR)/$(CATEGORY)/$(DEMO)

#*---------------------------------------------------------------------*/
#*    install                                                          */
#*---------------------------------------------------------------------*/
install:
	mkdir -p $(DESTDIR)
	install -m a+r $(POPULATION) $(DESTDIR)
