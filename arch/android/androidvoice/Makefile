#*=====================================================================*/
#*    .../project/hop/2.2.x/arch/android/androidvoice/Makefile         */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Fri Jan 20 14:35:57 2006                          */
#*    Last change :  Fri Oct 22 11:28:59 2010 (serrano)                */
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
build: etc/HopPluginVoice.dex etc/weblet.info etc/$(HZ).wiki config.hop .afile

clean:

distclean:
	$(RM) etc/weblet.info etc/$(HZ).wiki

#*---------------------------------------------------------------------*/
#*    Configuration                                                    */
#*---------------------------------------------------------------------*/
-include ../../../etc/Makefile.hopconfig
-include ../../../etc/Makefile.version
include ../../../etc/Makefile.weblet
-include $(BIGLOOLIBDIR)/Makefile.config

#*---------------------------------------------------------------------*/
#*    Compiling the Java file                                          */
#*---------------------------------------------------------------------*/
etc/HopPluginVoice.dex: etc/HopPluginVoice.java
	(cd etc && javac -classpath $(BUILDDIR)/arch/android/src -classpath $(DROIDROOT)/out/target/common/obj/JAVA_LIBRARIES/android_stubs_current_intermediates/classes HopPluginVoice.java)

