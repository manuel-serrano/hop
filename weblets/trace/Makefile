#*=====================================================================*/
#*    serrano/prgm/project/hop/1.10.x/weblets/trace/Makefile           */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Fri Jan 20 14:35:57 2006                          */
#*    Last change :  Mon Sep 29 09:48:22 2008 (serrano)                */
#*    Copyright   :  2006-08 Manuel Serrano                            */
#*    -------------------------------------------------------------    */
#*    The Makefile of the trace weblet                                 */
#*=====================================================================*/

#*---------------------------------------------------------------------*/
#*    CONFIG ...                                                       */
#*---------------------------------------------------------------------*/
TITLE = trace
VERSION = 1.0.0
MINHOP = $(HOPRELEASE) 
MAXHOP =
CATEGORY = hop
DATE = 18 april 2008

#*---------------------------------------------------------------------*/
#*    POPULATION                                                       */
#*---------------------------------------------------------------------*/
SOURCES = trace.hop
POPULATION = Makefile $(SOURCES) \
             etc/weblet.info.in etc/trace.wiki.in \
             etc/logo.png etc/favicon.png

build: etc/weblet.info etc/$(TITLE).wiki .afile

clean:

distclean:
	$(RM) etc/weblet.info etc/$(TITLE).wiki

#*---------------------------------------------------------------------*/
#*    Configuration                                                    */
#*---------------------------------------------------------------------*/
include ../../etc/Makefile.hopconfig
include ../../etc/Makefile.version
include ../../etc/Makefile.weblet

