#*=====================================================================*/
#*    serrano/prgm/project/hop/2.6.x/hopscript/Makefile                */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Wed Apr  1 18:45:46 1998                          */
#*    Last change :  Wed Jan 29 14:43:31 2014 (serrano)                */
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
HOPCLOSELIBS_S = $(BGLCLOSELIBS_S) -lscheme2js_u
HOPCLOSELIBS_U = $(BGLCLOSELIBS_U) -lscheme2js_u
HOPCLOSELIBS_ES = -lhopscript_s-$(HOPRELEASE) -lscheme2js_u
HOPCLOSELIBS_EU = -lhopscript_u-$(HOPRELEASE) -lscheme2js_u

BLIBFLAGS = $(BCFLAGSRTS)

#*---------------------------------------------------------------------*/
#*    Scheme extended objects                                          */
#*---------------------------------------------------------------------*/
_OBJECTS = types rts object function string number regexp boolean error \
  array arguments date json math private public lib service property

OBJECTS = $(_OBJECTS:%=o/%.o)
EOBJECTS = o/make_lib.o

BGL_CLASSES = $(_OBJECTS:%=$(CLASS_DIR)/%.class)
BGL_ECLASSES = $(CLASS_EDIR)/make_lib.class

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
.PHONY: build native jvm

build: $(BACKEND)

native: heap-c lib-c
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

#bdepend start (don't edit)
#*---------------------------------------------------------------------*/
#*    Dependencies ...                                                 */
#*---------------------------------------------------------------------*/


#bdepend stop
