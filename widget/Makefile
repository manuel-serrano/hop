#*=====================================================================*/
#*    serrano/prgm/project/hop/2.1.x/widget/Makefile                   */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Wed Apr  1 18:45:46 1998                          */
#*    Last change :  Fri Apr 23 08:11:15 2010 (serrano)                */
#*    -------------------------------------------------------------    */
#*    The makefile to build the HOP library.                           */
#*=====================================================================*/
.PHONY: do 

do: build

#*---------------------------------------------------------------------*/
#*    Standard Hop configuration                                       */
#*---------------------------------------------------------------------*/
-include ../etc/Makefile.hopconfig
-include ../etc/Makefile.version
-include $(BIGLOOLIBDIR)/Makefile.config

#*---------------------------------------------------------------------*/
#*    Compilers, Tools and Destinations                                */
#*---------------------------------------------------------------------*/
# the library name
TARGETNAME = hopwidget
HOPCLOSELIBS = $(BGLCLOSELIBS) -lhopscheme_s-$(HOPRELEASE) -lhop_s-$(HOPRELEASE) 
HOPCLOSELIBS_E = -lhopwidget_s-$(HOPRELEASE)

BLIBFLAGS = $(BCFLAGSRTS)

#*---------------------------------------------------------------------*/
#*    Scheme extended objects                                          */
#*---------------------------------------------------------------------*/
_OBJECTS = init notepad paned slider tree file foldlist tabslider \
   editor audio video lframe
_HOBJECTS= spinbutton colorchooser spage gauge

OBJECTS = $(_OBJECTS:%=o/%.o) $(_HOBJECTS:%=o/%.o)
EOBJECTS = o/make-lib.o

BGL_CLASSES = $(_OBJECTS:%=$(CLASS_DIR)/%.class) \
              $(_HOBJECTS:%=$(CLASS_DIR)/%.class)
BGL_ECLASSES = $(CLASS_EDIR)/make-lib.class

BGL_DOTNET_OBJ = $(_OBJECTS:%=$(DOTNET_OBJ_DIR)/%.obj) \
                 $(_HOBJECTS:%=$(DOTNET_OBJ_DIR)/%.obj)
BGL_DOTNET_EOBJ	= $(DOTNET_OBJ_EDIR)/make-lib.obj

SOURCES = $(_OBJECTS:%=%.scm) $(_HOBJECTS:%=%.hop)

INCLUDES = 

#*---------------------------------------------------------------------*/
#*    Sources                                                          */
#*---------------------------------------------------------------------*/
POPULATION = $(SOURCES) $(INCLUDES) make-lib.scm Makefile

include ../etc/Makefile.library

#*---------------------------------------------------------------------*/
#*    all                                                              */
#*---------------------------------------------------------------------*/
.PHONY: build native jvm dotnet distrib distrib-clean

build: 
	(LD_LIBRARY_PATH=$(BUILDLIBDIR):$$LD_LIBRARY_PATH; \
         export LD_LIBRARY_PATH; \
         DYLD_LIBRARY_PATH=$(BUILDLIBDIR):$$DYLD_LIBRARY_PATH; \
         export DYLD_LIBRARY_PATH; \
         PATH=$(BUILDBINDIR):$(BUILDLIBDIR):$$PATH; \
         export PATH; \
	 $(MAKE) $(BACKEND))

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
	@ $(AFILE) -suffix hop -o .afile $(SOURCES)

.jfile: $(SOURCES)
	@ $(JFILE) -suffix hop -o .jfile \
                   -pbase $(PBASE) $(SOURCES) make-lib.scm

.etags: $(SOURCES)
	@ $(BTAGS) -o .etags $(SOURCES)

getsources:
	@ echo $(SOURCES)

#*---------------------------------------------------------------------*/
#*    predistrib                                                       */
#*---------------------------------------------------------------------*/
predistrib: $(SOURCES:%.hop=%.scm)

predistrib-clean:
	$(RM) -f $(patsubst %.hop,%.scm,$(filter %hop, $(SOURCES)))

#*---------------------------------------------------------------------*/
#*    Explicit dependencies                                            */
#*---------------------------------------------------------------------*/

#bdepend start (don't edit)
#*---------------------------------------------------------------------*/
#*    Dependencies ...                                                 */
#*---------------------------------------------------------------------*/

#bdepend stop
