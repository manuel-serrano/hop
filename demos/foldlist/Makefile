#*=====================================================================*/
#*    serrano/prgm/project/hop/demos/widgets/foldlist/Makefile         */
#*    -------------------------------------------------------------    */
#*    Author      :  Erick Gallesio                                    */
#*    Creation    :  Tue Mar  7 14:36:14 2006                          */
#*    Last change :  Tue Mar 14 08:52:22 2006 (eg)                     */
#*    -------------------------------------------------------------    */
#*    Makefile of a HOP demo                                           */
#*=====================================================================*/

#*---------------------------------------------------------------------*/
#*    Configuration                                                    */
#*---------------------------------------------------------------------*/
include ../../etc/Makefile.hopconfig
include ../../etc/Makefile.version

FILES      = foldlist.hop demofl.css comment.hop
POPULATION = Makefile $(FILES)
DESTDIR    = $(HOPDEMOSDIR)/$(DEMO)

#*---------------------------------------------------------------------*/
#*    install                                                          */
#*---------------------------------------------------------------------*/
install:
	mkdir -p $(DESTDIR)
	install -m a+r $(POPULATION) $(DESTDIR)
