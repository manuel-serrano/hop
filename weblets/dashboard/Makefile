#*=====================================================================*/
#*    serrano/prgm/project/hop/1.10.x/weblets/dashboard/Makefile       */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Fri Jan 20 14:35:57 2006                          */
#*    Last change :  Mon Sep 29 09:48:54 2008 (serrano)                */
#*    Copyright   :  2006-08 Manuel Serrano                            */
#*    -------------------------------------------------------------    */
#*    The Makefile of the dashboard weblet                             */
#*=====================================================================*/

#*---------------------------------------------------------------------*/
#*    CONFIG ...                                                       */
#*---------------------------------------------------------------------*/
TITLE = dashboard
VERSION = 1.0.1
MINHOP = $(HOPRELEASE) 
MAXHOP =
CATEGORY = hop
DATE = 20 June 2008

#*---------------------------------------------------------------------*/
#*    POPULATION                                                       */
#*---------------------------------------------------------------------*/
SOURCES		= dashboard.hss dashboard.hop
POPULATION	= Makefile $(SOURCES) \
                  etc/weblet.info.in etc/dashboard.wiki.in \
                  dashboard-applet.hss etc/dashboard-rfc.png \
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

