#*=====================================================================*/
#*    serrano/prgm/project/hop/runtime/Makefile                        */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Wed Apr  1 18:45:46 1998                          */
#*    Last change :  Tue Mar 28 07:44:45 2006 (serrano)                */
#*    -------------------------------------------------------------    */
#*    The makefile to build the HOP library.                           */
#*=====================================================================*/
.PHONY: do 

do: build

#*---------------------------------------------------------------------*/
#*    Standard Bigloo configuration                                    */
#*---------------------------------------------------------------------*/
include ../etc/Makefile.hopconfig
include ../etc/Makefile.version
include $(BIGLOOLIBDIR)/Makefile.config

#*---------------------------------------------------------------------*/
#*    Compilers, Tools and Destinations                                */
#*---------------------------------------------------------------------*/
# the library name
TARGETNAME	= hop

#*---------------------------------------------------------------------*/
#*    Scheme extended objects                                          */
#*---------------------------------------------------------------------*/
_OBJECTS	= param misc types thread \
                  xml dom read \
                  read-js css js-lib cgi \
                  hop service \
                  job mime user \
                  http-lib http-response http-shoutcast http-error \
                  http-filter html-extra html-notepad html-tree html-slider \
                  html-paned html-tabslider html-inline html-foldlist \
                  html-window \
                  event color weblets prefs

OBJECTS		= $(_OBJECTS:%=o/%.o) o/configure.o
EOBJECTS	= o/make-lib.o

BGL_CLASSES	= $(_OBJECTS:%=$(CLASS_DIR)/%.class)
BGL_ECLASSES	= $(CLASS_EDIR)/make-lib.class

BGL_DOTNET_OBJ	= $(_OBJECTS:%=$(DOTNET_OBJ_DIR)/%.obj)
BGL_DOTNET_EOBJ	= $(DOTNET_OBJ_EDIR)/make-lib.obj

SOURCES		= $(_OBJECTS:%=%.scm)

INCLUDES	= compiler-macro.sch eval-macro.sch xml.sch param.sch \
                  http-lib.sch service.sch prefs.sch

#*---------------------------------------------------------------------*/
#*    Sources                                                          */
#*---------------------------------------------------------------------*/
POPULATION	= $(SOURCES) $(INCLUDES) make-lib.scm Makefile

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

.afile: $(SOURCES) configure.scm
	@ $(AFILE) -o .afile $(SOURCES) configure.scm

.jfile: $(SOURCES) configure.scm
	@ $(JFILE) -o .jfile \
                   -I $(MISC_SRC_DIR) \
                   -pbase $(PBASE) $(SOURCES) configure.scm make-lib.scm

.etags: $(SOURCES)
	@ $(BTAGS) -o .etags $(SOURCES) configure.scm

getsources:
	@ echo $(SOURCES)

#bdepend start (don't edit)
#*---------------------------------------------------------------------*/
#*    Dependencies ...                                                 */
#*---------------------------------------------------------------------*/
o/param.o param.class: ./eval-macro.sch ./param.sch 
o/html-slider.o html-slider.class: ./compiler-macro.sch ./xml.sch 
o/html-paned.o html-paned.class: ./compiler-macro.sch ./xml.sch 
o/http-response.o ./http-response.class: ./http-lib.sch 
o/param.o ./param.class: ./eval-macro.sch ./param.sch 
o/html-extra.o html-extra.class: ./compiler-macro.sch ./xml.sch 
o/event.o event.class: ./compiler-macro.sch ./xml.sch 
o/html-tree.o html-tree.class: ./compiler-macro.sch ./xml.sch 
o/http-response.o http-response.class: ./http-lib.sch 
o/http-lib.o ./http-lib.class: ./http-lib.sch 
o/service.o ./service.class: ./eval-macro.sch ./service.sch 
o/html-notepad.o html-notepad.class: ./compiler-macro.sch ./xml.sch 
o/service.o service.class: ./eval-macro.sch ./service.sch 
o/xml.o xml.class: ./eval-macro.sch ./xml.sch 
o/html-extra.o ./html-extra.class: ./compiler-macro.sch ./xml.sch 
o/builtin.o ./builtin.class: ./compiler-macro.sch ./service.sch 
o/html-tabslider.o html-tabslider.class: ./compiler-macro.sch ./xml.sch 
o/builtin.o builtin.class: ./compiler-macro.sch ./service.sch 
o/http-lib.o http-lib.class: ./http-lib.sch 
o/xml.o ./xml.class: ./eval-macro.sch ./xml.sch 
o/http-shoutcast.o http-shoutcast.class: ./http-lib.sch 
o/html-inline.o html-inline.class: ./compiler-macro.sch ./xml.sch 

#bdepend stop
