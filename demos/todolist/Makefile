#*=====================================================================*/
#*    serrano/prgm/project/hop/demos/widgets/calendar/Makefile         */
#*    -------------------------------------------------------------    */
#*    Author      :  Erick Gallesio                                    */
#*    Creation    :  Fri Mar  3 18:52:29 CET 2006                      */
#*    Last change :  Tue Mar 14 08:57:16 2006 (eg)                     */
#*    -------------------------------------------------------------    */
#*    The Makefile for the HOP demos/widgets/calendar directory        */
#*=====================================================================*/

#*---------------------------------------------------------------------*/
#*    Configuration                                                    */
#*---------------------------------------------------------------------*/
include ../../etc/Makefile.hopconfig
include ../../etc/Makefile.version

FILES      = todolist.hop
POPULATION = Makefile $(FILES)
DESTDIR    = $(HOPDEMOSDIR)/$(DEMO)

#*---------------------------------------------------------------------*/
#*    install                                                          */
#*---------------------------------------------------------------------*/
install:
	mkdir -p $(DESTDIR)
	install -m a+r $(POPULATION) $(DESTDIR)
