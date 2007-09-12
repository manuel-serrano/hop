#*=====================================================================*/
#*    serrano/prgm/project/hop/runtime/Makefile                        */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Wed Apr  1 18:45:46 1998                          */
#*    Last change :  Tue Sep 11 10:20:10 2007 (serrano)                */
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
TARGETNAME	= hop
HOPCLOSELIBS	= $(BGLCLOSELIBS)
HOPCLOSELIBS_E	= -lhop_s-$(HOPRELEASE)

#*---------------------------------------------------------------------*/
#*    Scheme extended objects                                          */
#*---------------------------------------------------------------------*/
_OBJECTS	= param misc types thread \
                  xml dom read \
                  read-js css js-lib cgi \
                  hop service \
                  job mime user cache \
                  http-lib http-request \
                  http-response http-shoutcast http-remote http-webdav \
                  http-error http-filter \
                  hop-extra hop-notepad hop-tree hop-slider \
                  hop-paned hop-tabslider hop-inline hop-foldlist \
                  hop-editor hop-fileselector hop-rss hop-sym hop-audio \
                  event color weblets prefs expanders  \
                  hop-wiki wiki-syntax wiki-toc

OBJECTS		= $(_OBJECTS:%=o/%.o) o/configure.o
EOBJECTS	= o/make-lib.o

BGL_CLASSES	= $(_OBJECTS:%=$(CLASS_DIR)/%.class) $(CLASS_DIR)/configure.class
BGL_ECLASSES	= $(CLASS_EDIR)/make-lib.class

BGL_DOTNET_OBJ	= $(_OBJECTS:%=$(DOTNET_OBJ_DIR)/%.obj)
BGL_DOTNET_EOBJ	= $(DOTNET_OBJ_EDIR)/make-lib.obj

SOURCES		= $(_OBJECTS:%=%.scm)

INCLUDES	= http-lib.sch \
                  expanders.sch \
                  xml.sch xml-expd.sch \
                  param.sch param-expd.sch \
                  service.sch service-expd.sch \
                  prefs-expd.sch  \
                  enable-thread.sch disable-thread.sch

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
                   -pbase $(PBASE) $(SOURCES) configure.scm make-lib.scm

.etags: $(SOURCES)
	@ $(BTAGS) -o .etags $(SOURCES) configure.scm

getsources:
	@ echo $(SOURCES)

#*---------------------------------------------------------------------*/
#*    Explicit dependencies                                            */
#*---------------------------------------------------------------------*/
o/thread.o thread.class: enable-thread.sch disable-thread.sch

#bdepend start (don't edit)
#*---------------------------------------------------------------------*/
#*    Dependencies ...                                                 */
#*---------------------------------------------------------------------*/
o/param.o param.class: param.sch 
o/hop-foldlist.o hop-foldlist.class: xml.sch 
o/http-response.o ./http-response.class: http-lib.sch 
o/hop-wiki.o hop-wiki.class: xml.sch 
o/param.o ./param.class: param.sch 
o/hop-editor.o hop-editor.class: xml.sch 
o/http-remote.o http-remote.class: http-lib.sch 
o/weblets.o weblets.class: xml.sch 
o/hop-fileselector.o hop-fileselector.class: xml.sch service.sch 
o/event.o event.class: xml.sch 
o/prefs.o prefs.class: xml.sch service.sch 
o/hop-inline.o hop-inline.class: xml.sch 
o/http-error.o http-error.class: xml.sch 
o/hop-slider.o hop-slider.class: xml.sch 
o/http-response.o http-response.class: http-lib.sch 
o/expanders.o expanders.class: service-expd.sch prefs-expd.sch           \
      param-expd.sch xml-expd.sch expanders.sch 
o/http-lib.o ./http-lib.class: http-lib.sch 
o/hop-extra.o ./hop-extra.class: xml.sch 
o/hop-paned.o hop-paned.class: xml.sch 
o/hop-tree.o ./hop-tree.class: xml.sch service.sch 
o/service.o ./service.class: service.sch 
o/service.o service.class: service.sch 
o/http-filter.o ./http-filter.class: http-lib.sch 
o/xml.o xml.class: param.sch xml.sch 
o/hop-extra.o hop-extra.class: xml.sch 
o/hop-tabslider.o hop-tabslider.class: xml.sch 
o/hop-notepad.o hop-notepad.class: xml.sch 
o/http-remote.o ./http-remote.class: http-lib.sch 
o/http-error.o ./http-error.class: xml.sch 
o/http-lib.o http-lib.class: http-lib.sch 
o/xml.o ./xml.class: param.sch xml.sch 
o/hop-tree.o hop-tree.class: xml.sch service.sch 
o/http-filter.o http-filter.class: http-lib.sch 
o/http-shoutcast.o http-shoutcast.class: http-lib.sch 

#bdepend stop
