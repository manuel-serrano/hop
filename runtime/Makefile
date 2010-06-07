#*=====================================================================*/
#*    serrano/prgm/project/hop/2.1.x/runtime/Makefile                  */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Wed Apr  1 18:45:46 1998                          */
#*    Last change :  Sat May 29 06:00:25 2010 (serrano)                */
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
TARGETNAME = hop
HOPCLOSELIBS = $(BGLCLOSELIBS) -lhopscheme_s-$(HOPRELEASE) 
HOPCLOSELIBS_E = -lhop_s-$(HOPRELEASE)

BLIBFLAGS = $(BCFLAGSRTS)

#*---------------------------------------------------------------------*/
#*    Scheme extended objects                                          */
#*---------------------------------------------------------------------*/
_OBJECTS = param misc priv types \
           xml html charset dom read module \
           read-js css cssmatch clientc js-lib json cgi \
           hop service \
           job mime user password cache \
           hop-wiki wiki-syntax wiki-parser wiki-toc hz \
           http-lib http-request \
           http-response http-shoutcast http-remote http-webdav \
           http-error http-filter \
           hop-img hop-extra hop-inline \
           hop-rss hop-sym \
           hop-svg hop-mathml \
           event color weblets prefs expanders security

OBJECTS = $(_OBJECTS:%=o/%.o) o/configure.o
EOBJECTS = o/make-lib.o

BGL_CLASSES = $(_OBJECTS:%=$(CLASS_DIR)/%.class) \
              $(CLASS_DIR)/configure.class
BGL_ECLASSES = $(CLASS_EDIR)/make-lib.class

BGL_DOTNET_OBJ = $(_OBJECTS:%=$(DOTNET_OBJ_DIR)/%.obj)
BGL_DOTNET_EOBJ	= $(DOTNET_OBJ_EDIR)/make-lib.obj

SOURCES = $(_OBJECTS:%=%.scm)

INCLUDES = http-lib.sch \
           expanders.sch \
           xml.sch xml-expd.sch \
           param.sch param-expd.sch \
           hss.sch hss-expd.sch \
           prefs.sch prefs-expd.sch  \
           service.sch service-expd.sch \
           enable-thread.sch disable-thread.sch

#*---------------------------------------------------------------------*/
#*    Sources                                                          */
#*---------------------------------------------------------------------*/
POPULATION = $(SOURCES) $(INCLUDES) make-lib.scm Makefile

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
	@ $(AFILE) -suffix hop -o .afile $(SOURCES) configure.scm

.jfile: $(SOURCES) configure.scm
	@ $(JFILE) -suffix hop -o .jfile \
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
o/hop-audio.o hop-audio.class: xml.sch service.sch 
o/hop-svg.o hop-svg.class: xml.sch 
o/param.o param.class: param.sch 
o/scm.o scm.class: xml.sch service.sch 
o/http-response.o ./http-response.class: http-lib.sch 
o/hop-wiki.o hop-wiki.class: xml.sch 
o/param.o ./param.class: param.sch 
o/http-webdav.o http-webdav.class: http-lib.sch xml.sch 
o/weblets.o ./weblets.class: xml.sch 
o/hop-editor.o hop-editor.class: xml.sch 
o/http-remote.o http-remote.class: http-lib.sch 
o/weblets.o weblets.class: xml.sch 
o/hop-fileselector.o hop-fileselector.class: xml.sch service.sch 
o/event.o event.class: xml.sch service.sch param.sch 
o/scm.o ./scm.class: xml.sch service.sch 
o/prefs.o prefs.class: xml.sch service.sch 
o/hop-inline.o hop-inline.class: xml.sch 
o/event.o ./event.class: xml.sch service.sch param.sch 
o/http-error.o http-error.class: xml.sch 
o/hop-slider.o hop-slider.class: xml.sch 
o/hop-mathml.o hop-mathml.class: xml.sch 
o/http-response.o http-response.class: http-lib.sch 
o/expanders.o expanders.class: service-expd.sch prefs-expd.sch           \
      param-expd.sch xml-expd.sch expanders.sch 
o/http-lib.o ./http-lib.class: http-lib.sch 
o/css.o css.class: xml.sch service.sch 
o/hop-extra.o ./hop-extra.class: xml.sch 
o/hop-paned.o hop-paned.class: xml.sch 
o/hop-tree.o ./hop-tree.class: xml.sch service.sch 
o/service.o ./service.class: service.sch 
o/service.o service.class: service.sch 
o/http-filter.o ./http-filter.class: http-lib.sch 
o/hop-img.o ./hop-img.class: param.sch xml.sch 
o/hop-img.o hop-img.class: param.sch xml.sch 
o/xml.o xml.class: param.sch xml.sch 
o/hop-extra.o hop-extra.class: xml.sch 
o/css.o ./css.class: xml.sch service.sch 
o/hop-rss.o hop-rss.class: xml.sch 
o/hop-notepad.o hop-notepad.class: xml.sch 
o/http-remote.o ./http-remote.class: http-lib.sch 
o/http-error.o ./http-error.class: xml.sch 
o/http-lib.o http-lib.class: http-lib.sch 
o/xml.o ./xml.class: param.sch xml.sch 
o/hop-slider.o ./hop-slider.class: xml.sch 
o/hop-tree.o hop-tree.class: xml.sch service.sch 
o/hop-video.o hop-video.class: xml.sch service.sch 
o/http-filter.o http-filter.class: http-lib.sch 
o/http-shoutcast.o http-shoutcast.class: http-lib.sch 

#bdepend stop
