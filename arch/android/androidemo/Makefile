#*=====================================================================*/
#*    .../prgm/project/hop/2.2.x/arch/android/androidemo/Makefile      */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Fri Jan 20 14:35:57 2006                          */
#*    Last change :  Tue Oct 12 17:58:26 2010 (serrano)                */
#*    Copyright   :  2006-10 Manuel Serrano                            */
#*    -------------------------------------------------------------    */
#*    Generic Makefile to build Hop weblets.                           */
#*=====================================================================*/
do: build

#*---------------------------------------------------------------------*/
#*    CONFIG ...                                                       */
#*---------------------------------------------------------------------*/
TITLE = androidemo
VERSION = 1.0.0
MINHOP = $(HOPRELEASE)
MAXHOP = $(HOPRELEASE)
CATEGORY = devel
DATE = $(HOPDATE)

#*---------------------------------------------------------------------*/
#*    POPULATION                                                       */
#*---------------------------------------------------------------------*/
SOURCES		= androidemo.hop config.hop.in

POPULATION	= Makefile androidemo.hss \
		  etc/weblet.info.in etc/androidemo.wiki.in \
                  etc/logo.png etc/favicon.png etc/doc.png \
                  $(SOURCES)

#*---------------------------------------------------------------------*/
#*    Targets                                                          */
#*---------------------------------------------------------------------*/
build: etc/weblet.info etc/$(TITLE).wiki config.hop .afile

clean:

distclean:
	$(RM) etc/weblet.info etc/$(TITLE).wiki

#*---------------------------------------------------------------------*/
#*    Configuration                                                    */
#*---------------------------------------------------------------------*/
-include ../../../etc/Makefile.hopconfig
-include ../../../etc/Makefile.version
-include ../../../etc/Makefile.weblet
-include $(BIGLOOLIBDIR)/Makefile.config
