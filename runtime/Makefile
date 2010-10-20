#*=====================================================================*/
#*    serrano/prgm/project/hop/2.2.x/runtime/Makefile                  */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Wed Apr  1 18:45:46 1998                          */
#*    Last change :  Wed Oct 20 09:23:41 2010 (serrano)                */
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
HOPCLOSELIBS_S = $(BGLCLOSELIBS_S) -lhopscheme_s-$(HOPRELEASE) 
HOPCLOSELIBS_U = $(BGLCLOSELIBS_U) -lhopscheme_u-$(HOPRELEASE) 
HOPCLOSELIBS_ES = -lhop_s-$(HOPRELEASE)
HOPCLOSELIBS_EU = -lhop_u-$(HOPRELEASE)

BLIBFLAGS = $(BCFLAGSRTS)

#*---------------------------------------------------------------------*/
#*    Scheme extended objects                                          */
#*---------------------------------------------------------------------*/
_OBJECTS = param misc priv types \
           xml xml_types html charset dom read module \
           read_js css cssmatch clientc js_lib json cgi \
           hop service \
           job mime user password cache \
           hop_wiki wiki_syntax wiki_parser wiki_toc hz \
           http_lib http_request \
           http_response http_shoutcast http_remote http_webdav \
           http_error http_filter \
           hop_img hop_extra hop_inline \
           hop_rss hop_sym \
           hop_svg hop_mathml \
           event color weblets prefs expanders security

OBJECTS = $(_OBJECTS:%=o/%.o) o/configure.o
EOBJECTS = o/make_lib.o

BGL_CLASSES = $(_OBJECTS:%=$(CLASS_DIR)/%.class) \
              $(CLASS_DIR)/configure.class
BGL_ECLASSES = $(CLASS_EDIR)/make_lib.class

BGL_DOTNET_OBJ = $(_OBJECTS:%=$(DOTNET_OBJ_DIR)/%.obj)
BGL_DOTNET_EOBJ	= $(DOTNET_OBJ_EDIR)/make_lib.obj

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
POPULATION = $(SOURCES) $(INCLUDES) make_lib.scm Makefile

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
                   -pbase $(PBASE) $(SOURCES) configure.scm make_lib.scm

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
o/http_filter.o ./http_filter.class: http-lib.sch 
o/param.o param.class: param.sch 
o/clientc.o ./clientc.class: xml.sch service.sch 
o/hop_extra.o ./hop_extra.class: xml.sch 
o/clientc.o clientc.class: xml.sch service.sch 
o/param.o ./param.class: param.sch 
o/weblets.o ./weblets.class: xml.sch 
o/weblets.o weblets.class: xml.sch 
o/http_response.o ./http_response.class: http-lib.sch 
o/hop_inline.o ./hop_inline.class: xml.sch 
o/hop_extra.o hop_extra.class: xml.sch 
o/http_remote.o ./http_remote.class: http-lib.sch 
o/configure.o ./configure.class: configure-noarch.sch 
o/http_lib.o ./http_lib.class: http-lib.sch 
o/event.o event.class: xml.sch service.sch param.sch 
o/prefs.o prefs.class: xml.sch service.sch 
o/http_filter.o http_filter.class: http-lib.sch 
o/http_error.o ./http_error.class: xml.sch 
o/hop_mathml.o ./hop_mathml.class: xml.sch 
o/prefs.o ./prefs.class: xml.sch service.sch 
o/hop_img.o ./hop_img.class: param.sch xml.sch 
o/hop_img.o hop_img.class: param.sch xml.sch 
o/http_webdav.o http_webdav.class: http-lib.sch xml.sch 
o/hop_rss.o hop_rss.class: xml.sch 
o/http_remote.o http_remote.class: http-lib.sch 
o/expanders.o expanders.class: service-expd.sch prefs-expd.sch           \
      param-expd.sch hss-expd.sch xml-expd.sch expanders.sch 
o/css.o css.class: xml.sch service.sch hss.sch 
o/service.o ./service.class: service.sch 
o/http_response.o http_response.class: http-lib.sch 
o/http_lib.o http_lib.class: http-lib.sch 
o/service.o service.class: service.sch 
o/hop_inline.o hop_inline.class: xml.sch 
o/hop_wiki.o hop_wiki.class: xml.sch 
o/xml.o xml.class: param.sch xml.sch 
o/css.o ./css.class: xml.sch service.sch hss.sch 
o/hop_svg.o hop_svg.class: xml.sch 
o/html.o ./html.class: param.sch xml.sch 
o/hop_mathml.o hop_mathml.class: xml.sch 
o/http_error.o http_error.class: xml.sch 
o/xml.o ./xml.class: param.sch xml.sch 
o/http_shoutcast.o http_shoutcast.class: http-lib.sch 
o/html.o html.class: param.sch xml.sch 

#bdepend stop
