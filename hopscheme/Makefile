#*=====================================================================*/
#*    serrano/prgm/project/hop/2.1.x/hopscheme/Makefile                */
#*    -------------------------------------------------------------    */
#*    Author      :  Florian Loitsch                                   */
#*    Creation    :  Wed Mar 15 07:27:50 2006                          */
#*    Last change :  Sun Jun 20 12:13:56 2010 (serrano)                */
#*    Copyright   :  2006-10 Manuel Serrano                            */
#*    -------------------------------------------------------------    */
#*    The Makefile to build the hopscheme library.                     */
#*=====================================================================*/
.PHONY: do

do: build

#*---------------------------------------------------------------------*/
#*    Standard Bigloo configuration                                    */
#*---------------------------------------------------------------------*/
-include ../etc/Makefile.hopconfig
-include ../etc/Makefile.version
-include $(BIGLOOLIBDIR)/Makefile.config

#*---------------------------------------------------------------------*/
#*    Compilers, Tools and Destinations                                */
#*---------------------------------------------------------------------*/
# the library name
TARGETNAME = hopscheme
HOPCLOSELIBS_S = $(BGLCLOSELIBS_S) -lscheme2js_s-$(HOPRELEASE)
HOPCLOSELIBS_U = $(BGLCLOSELIBS_U) -lscheme2js_u-$(HOPRELEASE)
HOPCLOSELIBS_ES = $(BGLCLOSELIBS_S) -lhopscheme_s-$(HOPRELEASE)
HOPCLOSELIBS_EU = $(BGLCLOSELIBS_S) -lhopscheme_u-$(HOPRELEASE)

BLIBFLAGS = $(BSCM2JSFLAGS)

#*---------------------------------------------------------------------*/
#*    Scheme extended objects                                          */
#*---------------------------------------------------------------------*/
_BGL_OBJECTS = hopscheme dollar_escape \
	       tilde_escape hopscheme_config hop_runtime \
	       dollar_scheme2js_module precompilation

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

include ../etc/Makefile.library

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
