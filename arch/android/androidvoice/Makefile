#*=====================================================================*/
#*    .../project/hop/2.2.x/arch/android/androidvoice/Makefile         */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Fri Jan 20 14:35:57 2006                          */
#*    Last change :  Fri Oct 22 10:55:52 2010 (serrano)                */
#*    Copyright   :  2006-10 Manuel Serrano                            */
#*    -------------------------------------------------------------    */
#*    Generic Makefile to build Hop weblets.                           */
#*=====================================================================*/
do: build

#*---------------------------------------------------------------------*/
#*    CONFIG ...                                                       */
#*---------------------------------------------------------------------*/
HZ = androidvoice
VERSION = 1.0.0
MINHOP = $(HOPRELEASE)
MAXHOP = $(HOPRELEASE)
CATEGORY = devel
DATE = $(HOPDATE)

#*---------------------------------------------------------------------*/
#*    POPULATION                                                       */
#*---------------------------------------------------------------------*/
SOURCES = androidvoice.hop config.hop.in

POPULATION = Makefile androidvoice.hss androidvoice.scm \
  etc/weblet.info.in etc/androidvoice.wiki.in \
  etc/logo.png etc/favicon.png etc/doc.png \
  etc/HopPluginVoice.java etc/HopPluginVoice.dex \
  $(SOURCES)

#*---------------------------------------------------------------------*/
#*    Targets                                                          */
#*---------------------------------------------------------------------*/
build: etc/weblet.info etc/$(HZ).wiki config.hop .afile \
  etc/HopPluginVoice.dex

clean:

distclean:
	$(RM) etc/weblet.info etc/$(HZ).wiki

#*---------------------------------------------------------------------*/
#*    Configuration                                                    */
#*---------------------------------------------------------------------*/
-include ../../../etc/Makefile.hopconfig
-include ../../../etc/Makefile.version
include ../../../etc/Makefile.weblet

#*---------------------------------------------------------------------*/
#*    Compiling the Java file                                          */
#*---------------------------------------------------------------------*/
etc/HopPluginVoice.dex: etc/HopPluginVoice.java
	java -classpath etc:$(BUILDDIR)/arch/android/src:$(DROIDROOT)/out/target/common/obj/JAVA_LIBRARIES/android_stubs_current_intermediates/classes

