#*=====================================================================*/
#*    serrano/prgm/project/hop/hop/arch/android/hopdroid/Makefile      */
#*    -------------------------------------------------------------    */
#*    Author      :  Florian Loitsch                                   */
#*    Creation    :  Wed Mar 15 07:27:50 2006                          */
#*    Last change :  Tue May 19 14:50:23 2020 (serrano)                */
#*    Copyright   :  2006-20 Manuel Serrano                            */
#*    -------------------------------------------------------------    */
#*    The Makefile to build the hopdroid library.                      */
#*=====================================================================*/
.PHONY: do

do: build

#*---------------------------------------------------------------------*/
#*    Standard Bigloo configuration                                    */
#*---------------------------------------------------------------------*/
-include ../../../etc/Makefile.hopconfig
-include ../../../etc/Makefile.version
-include $(BIGLOOLIBDIR)/Makefile.config

#*---------------------------------------------------------------------*/
#*    Compilers, Tools and Destinations                                */
#*---------------------------------------------------------------------*/
# the library name
TARGETNAME = hopdroid
HOPBIGLOOEXTRALIBS = bigloophone bigloomultimedia bigloomail 
HOPAPPLIBS = hop

#*---------------------------------------------------------------------*/
#*    Scheme extended objects                                          */
#*---------------------------------------------------------------------*/
_BGL_OBJECTS = music mpd multimedia tts wifi zeroconf system prefs

_OBJECTS = $(_BGL_OBJECTS)

OBJECTS = $(_OBJECTS:%=o/%.o) o/phone.o
EOBJECTS = o/make_lib.o

SOURCES_IN = $(_BGL_OBJECTS:%=%.scm)
SOURCES = $(SOURCES_IN) phone.scm

INCLUDES =

#*---------------------------------------------------------------------*/
#*    Sources                                                          */
#*---------------------------------------------------------------------*/
POPULATION = phone.scm.in $(SOURCES_IN) $(INCLUDES) make_lib.scm Makefile

include ../../../etc/Makefile.library

#*---------------------------------------------------------------------*/
#*    all                                                              */
#*---------------------------------------------------------------------*/
.PHONY: build native

build: $(BACKEND)

native: heap-c lib-c

install: install-native

#*---------------------------------------------------------------------*/
#*    ude                                                              */
#*---------------------------------------------------------------------*/
.PHONY: ude
ude:
	@ $(MAKE) -f Makefile .afile .etags .jfile
	@ $(MAKE) dep

.afile: $(SOURCES)
	@ $(AFILE) -o .afile $(SOURCES)

.etags: $(SOURCES)
	@ $(BTAGS) -o .etags $(SOURCES)

#bdepend start (don't edit)
#*---------------------------------------------------------------------*/
#*    Dependencies ...                                                 */
#*---------------------------------------------------------------------*/

#bdepend stop
