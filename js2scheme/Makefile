#*=====================================================================*/
#*    serrano/prgm/project/hop/hop/js2scheme/Makefile                  */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Wed Apr  1 18:45:46 1998                          */
#*    Last change :  Sun Apr 12 08:03:17 2020 (serrano)                */
#*    -------------------------------------------------------------    */
#*    The makefile to build the JS2SCHEME library.                     */
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
TARGETNAME = js2scheme
BIGLOOEXTRALIBS = biglooweb
HOPAPPLIBS = hop

BLIBFLAGS = $(BCFLAGSRTS)

#*---------------------------------------------------------------------*/
#*    Scheme extended objects                                          */
#*---------------------------------------------------------------------*/
_OBJECTS = lexer header parser html ast usage dump utils \
  stage compile js uninit globprop globvar varpreinit method nodesize \
  syntax return loopexit symbol ronly resolve this any letfun \
  use propcache bestpractice sourcemap debug stmtassign letopt unletrec \
  cps ecmascript5 narrow constant tyflow hint range loopspec cast vector \
  array alpha letfusion constrsize ctor propcce cspecs sweep inline unthis \
  instanceof hintnum pce callapply multivar module checksum \
  newtarget objinit arguments procedure cnstlift hop-module cse strbuffer \
  scheme scheme-utils scheme-program \
  scheme-fun scheme-ops scheme-test scheme-class scheme-string scheme-math \
  scheme-date scheme-array scheme-regexp scheme-cast scheme-arguments \
  scheme-call scheme-spread scheme-json scheme-bexit scheme-object \
  scheme-constant scheme-process scheme-tarray

OBJECTS = $(_OBJECTS:%=o/%.o)
EOBJECTS = o/make_lib.o

BGL_CLASSES = $(_OBJECTS:%=$(CLASS_DIR)/%.class)
BGL_ECLASSES = $(CLASS_EDIR)/make_lib.class

SOURCES = $(_OBJECTS:%=%.scm) ecmascript5.js

INCLUDES = ast.sch usage.sch usage-bit.sch scheme.sch

#*---------------------------------------------------------------------*/
#*    Sources                                                          */
#*---------------------------------------------------------------------*/
POPULATION = $(SOURCES) $(INCLUDES) make_lib.scm Makefile walk.sch token.sch

#*---------------------------------------------------------------------*/
#*    all                                                              */
#*---------------------------------------------------------------------*/
include ../etc/Makefile.library

.PHONY: build native jvm

build: $(BACKEND)

native: o heap-c lib-c
jvm: heap-jvm lib-jvm

lib-c: .afile

#*---------------------------------------------------------------------*/
#*    this-symbols.sch ...                                             */
#*---------------------------------------------------------------------*/
this-symbols.sch: $(BUILDETCDIR)/mkthissymbols.js
	$(HOP) --no-server $(BUILDETCDIR)/mkthissymbols.js > this-symbols.sch

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
#*    Explicit dependencies                                            */
#*---------------------------------------------------------------------*/
o/ecmascript5.o: ecmascript5.js

#bdepend start (don't edit)
#*---------------------------------------------------------------------*/
#*    Dependencies ...                                                 */
#*---------------------------------------------------------------------*/


#bdepend stop
