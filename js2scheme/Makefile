#*=====================================================================*/
#*    serrano/prgm/project/hop/3.2.x/js2scheme/Makefile                */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Wed Apr  1 18:45:46 1998                          */
#*    Last change :  Sun Sep 24 07:27:45 2017 (serrano)                */
#*    -------------------------------------------------------------    */
#*    The makefile to build the JS2SCHEME library.                     */
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
TARGETNAME = js2scheme
HOPCLOSELIBS_S = $(BGLCLOSELIBS_S) \
  -lhop_s-$(HOPRELEASE)
HOPCLOSELIBS_U = $(BGLCLOSELIBS_U) \
  -lhop_u-$(HOPRELEASE)
HOPCLOSELIBS_ES = -ljs2scheme_s-$(HOPRELEASE)
HOPCLOSELIBS_EU = -ljs2scheme_u-$(HOPRELEASE)

BLIBFLAGS = $(BCFLAGSRTS)

#*---------------------------------------------------------------------*/
#*    Scheme extended objects                                          */
#*---------------------------------------------------------------------*/
_OBJECTS = lexer header parser html ast dump utils \
  stage compile js globvar method nodesize \
  syntax return loopexit symbol ronly resolve this \
  use property bestpractice sourcemap debug stmtassign letopt \
  cps ecmascript5 narrow constant tyflow hint range cast \
  array alpha letfusion uint32 constrsize ctor propcce sweep imethod \
  scheme scheme-utils scheme-fun scheme-ops scheme-test scheme-class

OBJECTS = $(_OBJECTS:%=o/%.o)
EOBJECTS = o/make_lib.o

BGL_CLASSES = $(_OBJECTS:%=$(CLASS_DIR)/%.class)
BGL_ECLASSES = $(CLASS_EDIR)/make_lib.class

SOURCES = $(_OBJECTS:%=%.scm) ecmascript5.js

INCLUDES = ast.sch

#*---------------------------------------------------------------------*/
#*    Sources                                                          */
#*---------------------------------------------------------------------*/
POPULATION = $(SOURCES) $(INCLUDES) make_lib.scm Makefile walk.sch token.sch

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
#*    Explicit dependencies                                            */
#*---------------------------------------------------------------------*/
o/ecmascript5.o: ecmascript5.js

#bdepend start (don't edit)
#*---------------------------------------------------------------------*/
#*    Dependencies ...                                                 */
#*---------------------------------------------------------------------*/


#bdepend stop
