#*=====================================================================*/
#*    serrano/prgm/project/hop/demos/widgets/calendar/Makefile         */
#*    -------------------------------------------------------------    */
#*    Author      :  Erick Gallesio                                    */
#*    Creation    :  Fri Mar  3 18:52:29 CET 2006                      */
#*    Last change :  Sat Mar  4 11:49:19 2006 (eg)                     */
#*    -------------------------------------------------------------    */
#*    The Makefile for the HOP demos/widgets/calendar directory        */
#*=====================================================================*/

#*---------------------------------------------------------------------*/
#*    Configuration                                                    */
#*---------------------------------------------------------------------*/
include ../../../etc/Makefile.hopconfig
include ../../../etc/Makefile.version

FILES      = calendar.hop calendar.css calendar.hss
POPULATION = Makefile $(FILES)
DESTDIR    = $(HOPDEMOSDIR)/$(CATEGORY)/$(DEMO)

#*---------------------------------------------------------------------*/
#*    install                                                          */
#*---------------------------------------------------------------------*/
install:
	mkdir -p $(DESTDIR)
	install -m a+r $(POPULATION) $(DESTDIR)
