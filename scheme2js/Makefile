#*=====================================================================*/
#*    serrano/prgm/project/hop/scheme2js/Makefile                      */
#*    -------------------------------------------------------------    */
#*    Author      :  Florian Loitsch                                   */
#*    Creation    :  Wed Mar 15 07:27:50 2006                          */
#*    Last change :  Wed Mar 15 07:28:37 2006 (serrano)                */
#*    Copyright   :  2006 Manuel Serrano                               */
#*    -------------------------------------------------------------    */
#*    The Makefile to build the Scheme2JS library.                     */
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
TARGETNAME	= scheme2js

#*---------------------------------------------------------------------*/
#*    Scheme extended objects                                          */
#*---------------------------------------------------------------------*/
_BGL_OBJECTS	= capture collect-vars compile constants expanders \
	gen-code gen-js inline js-interface liveness node-elimination \
	nodes pobject-conv protobject scheme2js side statements symbol \
	tail-rec tail transform-util traverse var expand verbose dot-expand

_OBJECTS	= $(_BGL_OBJECTS)

OBJECTS		= $(_OBJECTS:%=o/%.o)
EOBJECTS	= o/make-lib.o

BGL_CLASSES	= $(_OBJECTS:%=$(CLASS_DIR)/%.class)
BGL_ECLASSES	= $(CLASS_EDIR)/make-lib.class

BGL_DOTNET_OBJ	= $(_OBJECTS:%=$(DOTNET_OBJ_DIR)/%.obj)
BGL_DOTNET_EOBJ	= $(DOTNET_OBJ_EDIR)/make-lib.obj

SOURCES		= $(_OBJECTS:%=%.scm)

INCLUDES	= hop-eval.sch nodes.sch protobject.sch tools.sch \
	runtime_mapping_hack.sch compile-optimized-boolify.scm \
	compile-optimized-call.scm

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
