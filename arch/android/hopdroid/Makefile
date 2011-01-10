#*=====================================================================*/
#*    .../prgm/project/hop/2.2.x/arch/android/hopdroid/Makefile        */
#*    -------------------------------------------------------------    */
#*    Author      :  Florian Loitsch                                   */
#*    Creation    :  Wed Mar 15 07:27:50 2006                          */
#*    Last change :  Sun Jan  9 18:23:17 2011 (serrano)                */
#*    Copyright   :  2006-11 Manuel Serrano                            */
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
HOPCLOSELIBS_S = $(BGLCLOSELIBS_S) \
  -lbigloophone_s-$(RELEASE) -lbigloomultimedia_s-$(RELEASE) \
  -lbigloomail_s-$(RELEASE) -lhop_s-$(HOPRELEASE) 
HOPCLOSELIBS_U = $(BGLCLOSELIBS_U) \
  -lbigloophone_u-$(RELEASE) -lbigloomultimedia_u-$(RELEASE) \
  -lbigloomail_u-$(RELEASE) -lhop_u-$(HOPRELEASE)
HOPCLOSELIBS_ES = -lhopdroid_s-$(HOPRELEASE)
HOPCLOSELIBS_EU = -lhopdroid_u-$(HOPRELEASE)

#*---------------------------------------------------------------------*/
#*    Scheme extended objects                                          */
#*---------------------------------------------------------------------*/
_BGL_OBJECTS = phone music multimedia tts

_OBJECTS = $(_BGL_OBJECTS)

OBJECTS = $(_OBJECTS:%=o/%.o)
EOBJECTS = o/make_lib.o

BGL_CLASSES = $(_OBJECTS:%=$(CLASS_DIR)/%.class)
BGL_ECLASSES = $(CLASS_EDIR)/make_lib.class

BGL_DOTNET_OBJ = $(_OBJECTS:%=$(DOTNET_OBJ_DIR)/%.obj)
BGL_DOTNET_EOBJ	= $(DOTNET_OBJ_EDIR)/make_lib.obj

SOURCES = $(_OBJECTS:%=%.scm)

INCLUDES =

#*---------------------------------------------------------------------*/
#*    Sources                                                          */
#*---------------------------------------------------------------------*/
POPULATION = $(SOURCES) $(INCLUDES) make_lib.scm Makefile

include ../../../etc/Makefile.library

#*---------------------------------------------------------------------*/
#*    all                                                              */
#*---------------------------------------------------------------------*/
.PHONY: build native jvm dotnet

build: $(BACKEND)

native: heap-c lib-c
jvm: heap-jvm lib-jvm
dotnet: heap-jvm lib-dotnet

#*---------------------------------------------------------------------*/
#*    ude                                                              */
#*---------------------------------------------------------------------*/
.PHONY: ude
ude:
	@ $(MAKE) -f Makefile .afile .etags .jfile
	@ $(MAKE) dep

.afile: $(SOURCES)
	@ $(AFILE) -o .afile $(SOURCES)

.jfile: $(SOURCES)
	@ $(JFILE) -o .jfile \
                   -pbase $(PBASE) $(SOURCES) make_lib.scm

.etags: $(SOURCES)
	@ $(BTAGS) -o .etags $(SOURCES)

#bdepend start (don't edit)
#*---------------------------------------------------------------------*/
#*    Dependencies ...                                                 */
#*---------------------------------------------------------------------*/

#bdepend stop
