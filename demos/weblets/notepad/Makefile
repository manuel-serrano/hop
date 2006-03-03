#*=====================================================================*/
#*    serrano/prgm/project/hop/demos/weblets/notepad/Makefile          */
#*    -------------------------------------------------------------    */
#*    Author      :  Erick Gallesio                                    */
#*    Creation    :  Wed Mar  1 14:25:06 2006                          */
#*    Last change :  Fri Mar  3 10:31:25 2006 (serrano)                */
#*    -------------------------------------------------------------    */
#*    The Makefile for the HOP demos/weblets/notepad directory         */
#*=====================================================================*/

#*---------------------------------------------------------------------*/
#*    Configuration                                                    */
#*---------------------------------------------------------------------*/
include ../../../etc/Makefile.hopconfig
include ../../../etc/Makefile.version

DESTDIR=$(HOPSHAREDIR)/demos/weblets/notepad

POPULATION = Makefile notepad.hop

#*---------------------------------------------------------------------*/
#*    install                                                          */
#*---------------------------------------------------------------------*/
install:
	mkdir -p $(DESTDIR)/nuvola/128x128 && \
	mkdir -p $(DESTDIR)/nuvola/16x16 && \
	install -m a+r $(POPULATION) $(DESTDIR)

