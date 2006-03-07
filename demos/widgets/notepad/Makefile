#*=====================================================================*/
#*    serrano/prgm/project/hop/demos/widgets/notepad/Makefile          */
#*    -------------------------------------------------------------    */
#*    Author      :  Erick Gallesio                                    */
#*    Creation    :  Wed Mar  1 14:25:06 2006                          */
#*    Last change :  Tue Mar  7 10:14:51 2006 (eg)                     */
#*    -------------------------------------------------------------    */
#*    The Makefile for the HOP demos/widgets/notepad directory         */
#*=====================================================================*/

#*---------------------------------------------------------------------*/
#*    Configuration                                                    */
#*---------------------------------------------------------------------*/
include ../../../etc/Makefile.hopconfig
include ../../../etc/Makefile.version

FILES      = notepad.hop
POPULATION = Makefile $(FILES)
DESTDIR    = $(HOPDEMOSDIR)/$(CATEGORY)/$(DEMO)

#*---------------------------------------------------------------------*/
#*    install                                                          */
#*---------------------------------------------------------------------*/
install:
	mkdir -p $(DESTDIR) $(DESTDIR)/nuvola/128x128 $(DESTDIR)/nuvola/16x16
	install -m a+r $(FILES) $(DESTDIR)
	install -m a+r nuvola/128x128/* $(DESTDIR)/nuvola/128x128
	install -m a+r nuvola/16x16/* $(DESTDIR)/nuvola/16x16
