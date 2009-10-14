#*=====================================================================*/
#*    serrano/prgm/project/hop/2.0.x/weblets/color/Makefile            */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Fri Jan 20 14:35:57 2006                          */
#*    Last change :  Wed Oct 14 11:40:01 2009 (serrano)                */
#*    Copyright   :  2006-09 Manuel Serrano                            */
#*    -------------------------------------------------------------    */
#*    The Makefile of the dashboard weblet                             */
#*=====================================================================*/

#*---------------------------------------------------------------------*/
#*    CONFIG ...                                                       */
#*---------------------------------------------------------------------*/
TITLE = color
VERSION = 1.0.0
MINHOP = $(HOPRELEASE) 
MAXHOP =
CATEGORY = hop
DATE = 26 June 2009

#*---------------------------------------------------------------------*/
#*    POPULATION                                                       */
#*---------------------------------------------------------------------*/
SOURCES		= color.hss color.hop config.hop.in
POPULATION	= Makefile $(SOURCES) \
                  etc/weblet.info.in etc/color.wiki.in \
                  etc/logo.png etc/dashboard.png etc/favicon.png \
                  etc/doc.png etc/grab.png

build: etc/weblet.info etc/$(TITLE).wiki config.hop .afile

clean:

distclean:
	$(RM) etc/weblet.info etc/$(TITLE).wiki

#*---------------------------------------------------------------------*/
#*    Configuration                                                    */
#*---------------------------------------------------------------------*/
-include ../../etc/Makefile.hopconfig
-include ../../etc/Makefile.version
include ../../etc/Makefile.weblet

