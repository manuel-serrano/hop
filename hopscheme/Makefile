#*=====================================================================*/
#*    serrano/prgm/project/hop/1.10.x/hopscheme/Makefile               */
#*    -------------------------------------------------------------    */
#*    Author      :  Florian Loitsch                                   */
#*    Creation    :  Wed Mar 15 07:27:50 2006                          */
#*    Last change :  Mon Dec  1 14:35:26 2008 (serrano)                */
#*    Copyright   :  2006-08 Manuel Serrano                            */
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
HOPCLOSELIBS = $(BGLCLOSELIBS) -lscheme2js_s-$(HOPRELEASE)
HOPCLOSELIBS_E = -lhopscheme_s-$(HOPRELEASE)

BLIBFLAGS = $(BSCM2JSFLAGS)

#*---------------------------------------------------------------------*/
#*    Scheme extended objects                                          */
#*---------------------------------------------------------------------*/
_BGL_OBJECTS = hopscheme scm-compil dollar-escape \
	       tilde-escape hopscheme-config hop-exports

_OBJECTS = $(_BGL_OBJECTS)

OBJECTS = $(_OBJECTS:%=o/%.o)
EOBJECTS = o/make-lib.o

BGL_CLASSES = $(_OBJECTS:%=$(CLASS_DIR)/%.class)
BGL_ECLASSES = $(CLASS_EDIR)/make-lib.class

BGL_DOTNET_OBJ = $(_OBJECTS:%=$(DOTNET_OBJ_DIR)/%.obj)
BGL_DOTNET_EOBJ	= $(DOTNET_OBJ_EDIR)/make-lib.obj

SOURCES = $(_OBJECTS:%=%.scm)

INCLUDES = 

#*---------------------------------------------------------------------*/
#*    Sources                                                          */
#*---------------------------------------------------------------------*/
POPULATION = $(SOURCES) $(INCLUDES) make-lib.scm Makefile

include ../etc/Makefile.library

#*---------------------------------------------------------------------*/
#*    hop-runtime.sch ...                                              */
#*---------------------------------------------------------------------*/
HOP_SHARE = ../share
HOP_RUNTIME_JS = $(HOP_SHARE)/hop-runtime.js

SCHEME2JS_RUNTIME = ../scheme2js/runtime
include $(SCHEME2JS_RUNTIME)/Makefile.include
EXPORTER = $(SCHEME2JS_RUNTIME)/exporter
EXPORTER_FLAGS = --module "hop-runtime" --scheme2js-modules --constant

hop-runtime.sch: $(EXPORTER) $(HOP_RUNTIME_JS)
	$(EXPORTER) $(EXPORTER_FLAGS) -o $@ $(HOP_RUNTIME_JS)

#*---------------------------------------------------------------------*/
#*    additional dependency                                            */
#*---------------------------------------------------------------------*/
o/hop-exports.o: hop-runtime.sch
o/class_s/bigloo/hopscheme/hop-exports.class: hop-runtime.sch

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
                   -pbase $(PBASE) $(SOURCES) make-lib.scm

.etags: $(SOURCES)
	@ $(BTAGS) -o .etags $(SOURCES)

#bdepend start (don't edit)
#*---------------------------------------------------------------------*/
#*    Dependencies ...                                                 */
#*---------------------------------------------------------------------*/

#bdepend stop
