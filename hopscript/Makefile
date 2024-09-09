#*=====================================================================*/
#*    serrano/prgm/project/hop/hop/hopscript/Makefile                  */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Wed Apr  1 18:45:46 1998                          */
#*    Last change :  Mon Sep  9 08:30:05 2024 (serrano)                */
#*    -------------------------------------------------------------    */
#*    The makefile to build the HOPSCRIPT library.                     */
#*=====================================================================*/
.PHONY: do install

do: build
install: install-library
	cp bglhopscript.h \
           $(DESTDIR)$(HOPLIBDIR)/hop/$(HOPRELEASE)/bglhopscript.h
	chmod $(MODFILE) $(DESTDIR)$(HOPLIBDIR)/hop/$(HOPRELEASE)/bglhopscript.h
	cp bglhopscript_malloc.h \
           $(DESTDIR)$(HOPLIBDIR)/hop/$(HOPRELEASE)/bglhopscript_malloc.h
	chmod $(MODFILE) $(DESTDIR)$(HOPLIBDIR)/hop/$(HOPRELEASE)/bglhopscript_malloc.h
	cp bglhopscript_rewrite.h \
           $(DESTDIR)$(HOPLIBDIR)/hop/$(HOPRELEASE)/bglhopscript_rewrite.h
	chmod $(MODFILE) $(DESTDIR)$(HOPLIBDIR)/hop/$(HOPRELEASE)/bglhopscript_rewrite.h

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
HOPAPPLIBS = js2scheme hop 

BLIBFLAGS = $(BCFLAGSRTS)

#*---------------------------------------------------------------------*/
#*    Scheme extended objects                                          */
#*---------------------------------------------------------------------*/
_OBJECTS = types object function string number regexp boolean \
  error array arguments date json math worker private public lib service \
  property websocket pair obj arraybuffer arraybufferview stringliteral dom \
  symbol promise generator proxy reflect map set spawn expanders profile \
  arithmetic arithmetic32 arithmetic64 builtin names vector bigint url

_C_OBJECTS = _bglhopscript

OBJECTS = $(_OBJECTS:%=o/%.o) $(_C_OBJECTS:%=o/%.o)
EOBJECTS = o/make_lib.o

BGL_CLASSES = $(_OBJECTS:%=$(CLASS_DIR)/%.class)
BGL_ECLASSES = $(CLASS_EDIR)/make_lib.class

SOURCES = $(_OBJECTS:%=%.scm) $(_C_OBJECTS:%=%.c)

INCLUDES = stringliteral.sch names.sch property_expd.sch property.sch \
  expanders.sch arithmetic.sch call.sch array.sch number.sch public_expd.sch \
  stringliteral_expd.sch types.sch types_expd.sch arity.sch \
  constants_expd.sch constants.sch stringthread.sch function.sch arguments.sch \
  export_expd.sch export.sch

C_INCLUDES = bglhopscript.h bglhopscript_types.h bglhopscript_malloc.h

#*---------------------------------------------------------------------*/
#*    Sources                                                          */
#*---------------------------------------------------------------------*/
POPULATION = $(SOURCES) $(INCLUDES) make_lib.scm Makefile

#*---------------------------------------------------------------------*/
#*    all                                                              */
#*---------------------------------------------------------------------*/
include ../etc/Makefile.library

.PHONY: build native jvm

build: .afile
	$(MAKE) $(BACKEND)

native: o heap-c lib-c
jvm: heap-jvm lib-jvm

heap-c: .afile
lib-c: .afile

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
#*    bglhopscript_types.h                                             */
#*---------------------------------------------------------------------*/
bglhopscript_types.h: types.scm
	$(BIGLOO) $(EFLAGS) $(BCFLAGS) -copt $(CPICFLAGS) -copt $(CNANFLAGS) -copt "-I$(C_SRC_DIR)" -copt "$(CEFLAGS)" $< -o $@ -hgen

#*---------------------------------------------------------------------*/
#*    Explicit dependencies                                            */
#*---------------------------------------------------------------------*/
o/%.o: .afile

o/_bglhopscript.o: _bglhopscript.c bglhopscript_types.h bglhopscript_malloc.h
	@ $(call compile2,$(CCOMP),$(CCFLAGS), -c $< -o $@)

o/expanders.o: expanders.sch public_expd.sch property_expd.sch \
  arithmetic.sch array.sch number.sch call.sch function.sch arguments.sch \
  stringliteral_expd.sch types_expd.sch constants_expd.sch names_expd.sch
o/function.o: types.sch stringliteral.sch property.sch function.sch

#bdepend start (don't edit)
#*---------------------------------------------------------------------*/
#*    Dependencies ...                                                 */
#*---------------------------------------------------------------------*/


#bdepend stop
