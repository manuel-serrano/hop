#*=====================================================================*/
#*    serrano/prgm/project/hop/demos/widgets/notepad/Makefile          */
#*    -------------------------------------------------------------    */
#*    Author      :  Erick Gallesio                                    */
#*    Creation    :  Wed Mar  1 14:25:06 2006                          */
#*    Last change :  Fri Mar  3 15:55:45 2006 (eg)                     */
#*    -------------------------------------------------------------    */
#*    The Makefile for the HOP demos/widgets/notepad directory         */
#*=====================================================================*/

#*---------------------------------------------------------------------*/
#*    Configuration                                                    */
#*---------------------------------------------------------------------*/
include ../../../etc/Makefile.hopconfig
include ../../../etc/Makefile.version

DESTDIR=$(HOPSHAREDIR)/demos/widgets/notepad

POPULATION = Makefile notepad.hop

#*---------------------------------------------------------------------*/
#*    install                                                          */
#*---------------------------------------------------------------------*/
install:
	mkdir -p $(DESTDIR)/nuvola/128x128 && \
	mkdir -p $(DESTDIR)/nuvola/16x16 && \
	install -m a+r $(POPULATION) $(DESTDIR)

