#*=====================================================================*/
#*    serrano/prgm/project/hop/3.1.x/hopscript/Makefile                */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Wed Apr  1 18:45:46 1998                          */
#*    Last change :  Wed Mar  9 10:51:17 2016 (serrano)                */
#*    -------------------------------------------------------------    */
#*    The makefile to build the HOPSCRIPT library.                     */
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
TARGETNAME = hopscript
HOPCLOSELIBS_S = $(BGLCLOSELIBS_S) \
  -ljs2scheme_s-$(HOPRELEASE) \
   -lhop_s-$(HOPRELEASE) \
  -lhopscheme_s-$(HOPRELEASE) \
  -lhopwidget_s-$(HOPRELEASE)
HOPCLOSELIBS_U = $(BGLCLOSELIBS_U) \
  -ljs2scheme_u-$(HOPRELEASE) \
  -lhop_u-$(HOPRELEASE) \
  -lhopscheme_s-$(HOPRELEASE) \
  -lhopwidget_u-$(HOPRELEASE)
HOPCLOSELIBS_ES = -lhopscript_s-$(HOPRELEASE)
HOPCLOSELIBS_EU = -lhopscript_u-$(HOPRELEASE)

BLIBFLAGS = $(BCFLAGSRTS)

#*---------------------------------------------------------------------*/
#*    Scheme extended objects                                          */
#*---------------------------------------------------------------------*/
_OBJECTS = types rts object function string number regexp boolean \
  error array arguments date json math worker private public lib service \
  property websocket pair arraybuffer arraybufferview stringliteral dom \
  symbol promise generator

_C_OBJECTS = _bglhopscript

OBJECTS = $(_OBJECTS:%=o/%.o) $(_C_OBJECTS:%=o/%.o)
EOBJECTS = o/make_lib.o

BGL_CLASSES = $(_OBJECTS:%=$(CLASS_DIR)/%.class)
BGL_ECLASSES = $(CLASS_EDIR)/make_lib.class

SOURCES = $(_OBJECTS:%=%.scm) $(_C_OBJECTS:%=%.c)

INCLUDES = stringliteral.sch property_expd.sch property.sch expanders.sch
C_INCLUDES = bglhopscript.h

#*---------------------------------------------------------------------*/
#*    Sources                                                          */
#*---------------------------------------------------------------------*/
POPULATION = $(SOURCES) $(INCLUDES) make_lib.scm Makefile

include ../etc/Makefile.library

#*---------------------------------------------------------------------*/
#*    all                                                              */
#*---------------------------------------------------------------------*/
.PHONY: build native jvm

build: $(BACKEND)

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

.etags: $(SOURCES) $(INCLUDES)
	@ $(BTAGS) -o .etags $(SOURCES) $(INCLUDES)

getsources:
	@ echo $(SOURCES)

#*---------------------------------------------------------------------*/
#*    bglhopscript.h                                                   */
#*---------------------------------------------------------------------*/
bglhopscript.h: types.scm
	$(BIGLOO) $(EFLAGS) $(BCFLAGS) -copt $(CPICFLAGS) -copt "-I$(C_SRC_DIR)" -copt "$(CEFLAGS)" $< -o $@ -hgen

#*---------------------------------------------------------------------*/
#*    Explicit dependencies                                            */
#*---------------------------------------------------------------------*/
o/_bglhopscript.o: _bglhopscript.c bglhopscript.h
	@ $(call compile2,$(CCOMP),$(CCFLAGS), -c $< -o $@)

#bdepend start (don't edit)
#*---------------------------------------------------------------------*/
#*    Dependencies ...                                                 */
#*---------------------------------------------------------------------*/


#bdepend stop
