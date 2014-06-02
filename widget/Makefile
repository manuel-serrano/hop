#*=====================================================================*/
#*    serrano/prgm/project/hop/3.0.x/widget/Makefile                   */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Wed Apr  1 18:45:46 1998                          */
#*    Last change :  Sat May 24 06:48:25 2014 (serrano)                */
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
HOPCLOSELIBS_S = $(BGLCLOSELIBS_S) \
  -lhopscheme_s-$(HOPRELEASE) -lhop_s-$(HOPRELEASE) 
HOPCLOSELIBS_U = $(BGLCLOSELIBS_U) \
  -lhopscheme_u-$(HOPRELEASE) -lhop_u-$(HOPRELEASE) 
HOPCLOSELIBS_ES = -lhopwidget_s-$(HOPRELEASE)
HOPCLOSELIBS_EU = -lhopwidget_u-$(HOPRELEASE)

BLIBFLAGS = $(BCFLAGSRTS)

#*---------------------------------------------------------------------*/
#*    Scheme extended objects                                          */
#*---------------------------------------------------------------------*/
_OBJECTS = init notepad paned slider tree file foldlist tabslider \
   editor audio video lframe foot sorttable sym prefs
_HOBJECTS= spinbutton colorchooser spage gauge

OBJECTS = $(_OBJECTS:%=o/%.o) $(_HOBJECTS:%=o/%.o)
EOBJECTS = o/make_lib.o

BGL_CLASSES = $(_OBJECTS:%=$(CLASS_DIR)/%.class) \
              $(_HOBJECTS:%=$(CLASS_DIR)/%.class)
BGL_ECLASSES = $(CLASS_EDIR)/make_lib.class

SOURCES = $(_OBJECTS:%=%.scm) $(_HOBJECTS:%=%.hop)

INCLUDES = 

#*---------------------------------------------------------------------*/
#*    Sources                                                          */
#*---------------------------------------------------------------------*/
POPULATION = $(SOURCES) $(INCLUDES) make_lib.scm Makefile

include ../etc/Makefile.library

#*---------------------------------------------------------------------*/
#*    all                                                              */
#*---------------------------------------------------------------------*/
.PHONY: build native jvm distrib distrib-clean

build: .afile
	(LD_LIBRARY_PATH=$(BUILDLIBDIR):$$LD_LIBRARY_PATH; \
         export LD_LIBRARY_PATH; \
         DYLD_LIBRARY_PATH=$(BUILDLIBDIR):$$DYLD_LIBRARY_PATH; \
         export DYLD_LIBRARY_PATH; \
         PATH=$(BUILDBINDIR):$(BUILDLIBDIR):$$PATH; \
         export PATH; \
	 $(MAKE) $(BACKEND))

native: o heap-c lib-c
jvm: heap-jvm lib-jvm

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
                   -pbase $(PBASE) $(SOURCES) make_lib.scm

.etags: $(SOURCES)
	@ $(BTAGS) -o .etags $(SOURCES)

getsources:
	@ echo $(SOURCES)

#*---------------------------------------------------------------------*/
#*    predistrib                                                       */
#*---------------------------------------------------------------------*/
.PHONY: predistrib predistrib-src predistrib-clean

predistrib:
	(LD_LIBRARY_PATH=$(BUILDLIBDIR):$$LD_LIBRARY_PATH; \
         export LD_LIBRARY_PATH; \
         DYLD_LIBRARY_PATH=$(BUILDLIBDIR):$$DYLD_LIBRARY_PATH; \
         export DYLD_LIBRARY_PATH; \
         PATH=$(BUILDBINDIR):$(BUILDLIBDIR):$$PATH; \
         export PATH; \
	 $(MAKE) predistrib-src)

predistrib-src: $(SOURCES:%.hop=%.scm)

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
