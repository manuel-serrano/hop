#*=====================================================================*/
#*    serrano/prgm/project/hop/demos/weblets/notepad/Makefile          */
#*    -------------------------------------------------------------    */
#*    Author      :  Erick Gallesio                                    */
#*    Creation    :  Wed Mar  1 14:25:06 2006                          */
#*    Last change :  Thu Mar  2 14:37:38 2006 (eg)                     */
#*    -------------------------------------------------------------    */
#*    The Makefile for the HOP demos/weblets/notepad directory         */
#*=====================================================================*/

#*---------------------------------------------------------------------*/
#*    Configuration                                                    */
#*---------------------------------------------------------------------*/
include ../../../etc/Makefile.hopconfig
include ../../../etc/Makefile.version

DESTDIR=$(HOPSHAREDIR)/demos/weblets/notepad

POPULATION = notepad.hop

#*---------------------------------------------------------------------*/
#*    install                                                          */
#*---------------------------------------------------------------------*/
install:
	mkdir -p $(DESTDIR)/nuvola/128x128 && \
	mkdir -p $(DESTDIR)/nuvola/16x16 && \
	install -m a+r $(POPULATION) $(DESTDIR)

