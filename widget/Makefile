#*=====================================================================*/
#*    serrano/prgm/project/hop/hop/widget/Makefile                     */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Wed Apr  1 18:45:46 1998                          */
#*    Last change :  Thu Feb 16 07:27:00 2023 (serrano)                */
#*    -------------------------------------------------------------    */
#*    The makefile to build the HOP library.                           */
#*=====================================================================*/
.PHONY: do install

do: build
install: install-library

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
HOPAPPLIBS = hop hopscheme

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

#*---------------------------------------------------------------------*/
#*    all                                                              */
#*---------------------------------------------------------------------*/
include ../etc/Makefile.library

.PHONY: build native jvm distrib distrib-clean

build: .afile $(BACKEND)

native: o heap-c lib-c
jvm: heap-jvm lib-jvm

heap-c: .afile
lib-c: o heap-c .afile

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

predistrib: $(SOURCES:%.hop=%.scm)

predistrib-clean:
	$(RM) -f $(patsubst %.hop,%.scm,$(filter %hop, $(SOURCES)))

#*---------------------------------------------------------------------*/
#*    Explicit dependencies                                            */
#*---------------------------------------------------------------------*/
o/%.o: .afile

colorchooser.o: colorchooser.hop
colorchooser.scm: colorchooser.hop

spinbutton.o: spinbutton.hop
spinbutton.scm: spinbutton.hop

gauge.o: gauge.hop
gauge.scm: gauge.hop

#bdepend start (don't edit)
#*---------------------------------------------------------------------*/
#*    Dependencies ...                                                 */
#*---------------------------------------------------------------------*/

#bdepend stop
