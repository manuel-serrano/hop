#*=====================================================================*/
#*    serrano/prgm/project/hop/2.4.x/runtime/Makefile                  */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Wed Apr  1 18:45:46 1998                          */
#*    Last change :  Thu Aug 16 08:51:52 2012 (serrano)                */
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
_OBJECTS = param misc priv types read module \
           xml xml_types charset dom \
           html_base html_head html_img \
           svg mathml \
           read_js clientc js_comp json cgi \
           css cssmatch hop_rss \
           hop service \
           job mime user password cache \
           hop_wiki wiki_syntax wiki_parser wiki_toc hz \
           http_lib http_request \
           http_response http_shoutcast http_proxy http_webdav \
           http_error http_filter \
           hop_inline \
           event websocket color weblets preferences expanders \
           security xdomain zeroconf

OBJECTS = $(_OBJECTS:%=o/%.o) o/configure.o
EOBJECTS = o/make_lib.o

BGL_CLASSES = $(_OBJECTS:%=$(CLASS_DIR)/%.class) \
              $(CLASS_DIR)/configure.class
BGL_ECLASSES = $(CLASS_EDIR)/make_lib.class

BGL_DOTNET_OBJ = $(_OBJECTS:%=$(DOTNET_OBJ_DIR)/%.obj)
BGL_DOTNET_EOBJ	= $(DOTNET_OBJ_EDIR)/make_lib.obj

SOURCES = $(_OBJECTS:%=%.scm)

INCLUDES = http_lib.sch \
           expanders.sch \
           xml.sch xml_expd.sch \
           param.sch param_expd.sch \
           hss.sch hss_expd.sch \
           prefs.sch prefs_expd.sch  \
           service.sch service_expd.sch \
           verbose.sch verbose_expd.sch \
           enable_thread.sch disable_thread.sch \
           wiki_syntax.sch wiki_parser.sch \
           zeroconf_avahi.sch zeroconf_dummy.sch

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

.etags: $(SOURCES) $(INCLUDES)
	@ $(BTAGS) -o .etags $(SOURCES) $(INCLUDES) configure.scm

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
o/http_error.o ./http_error.class: xml.sch 
o/http_filter.o http_filter.class: http_lib.sch 
o/wiki_parser.o wiki_parser.class: wiki_syntax.sch wiki_parser.sch 
o/mathml.o ./mathml.class: xml.sch 
o/clientc.o ./clientc.class: xml.sch service.sch 
o/http_lib.o http_lib.class: http_lib.sch 
o/security.o ./security.class: param.sch 
o/preferences.o preferences.class: xml.sch service.sch 
o/service.o service.class: service.sch 
o/xdomain.o xdomain.class: service.sch 
o/xml.o ./xml.class: param.sch xml.sch 
o/http_response.o http_response.class: http_lib.sch 
o/http_response.o ./http_response.class: http_lib.sch 
o/expanders.o expanders.class: service_expd.sch prefs_expd.sch           \
      param_expd.sch hss_expd.sch xml_expd.sch expanders.sch 
o/misc.o ./misc.class: param.sch 
o/http_error.o http_error.class: xml.sch 
o/css.o ./css.class: xml.sch service.sch hss.sch 
o/mathml.o mathml.class: xml.sch 
o/event.o event.class: xml.sch service.sch param.sch 
o/event.o ./event.class: xml.sch service.sch param.sch 
o/http_webdav.o http_webdav.class: http_lib.sch xml.sch 
o/weblets.o weblets.class: xml.sch 
o/http_lib.o ./http_lib.class: http_lib.sch 
o/html_img.o html_img.class: param.sch xml.sch 
o/html_base.o html_base.class: param.sch xml.sch 
o/svg.o ./svg.class: xml.sch 
o/html_base.o ./html_base.class: param.sch xml.sch 
o/http_remote.o ./http_remote.class: http_lib.sch 
o/html_head.o html_head.class: xml.sch service.sch 
o/html_head.o ./html_head.class: xml.sch service.sch 
o/hop_inline.o ./hop_inline.class: xml.sch 
o/http_filter.o ./http_filter.class: http_lib.sch 
o/clientc.o clientc.class: xml.sch service.sch 
o/configure.o ./configure.class: configure_noarch.sch 
o/wiki_parser.o ./wiki_parser.class: wiki_syntax.sch wiki_parser.sch 
o/hop_wiki.o hop_wiki.class: xml.sch 
o/xml.o xml.class: param.sch xml.sch 
o/preferences.o ./preferences.class: xml.sch service.sch 
o/service.o ./service.class: service.sch 
o/html_img.o ./html_img.class: param.sch xml.sch 
o/xdomain.o ./xdomain.class: service.sch 
o/css.o css.class: xml.sch service.sch hss.sch 
o/zeroconf.o zeroconf.class: zeroconf_avahi.sch
o/hop_inline.o hop_inline.class: xml.sch 
o/hop_rss.o hop_rss.class: xml.sch 
o/http_shoutcast.o http_shoutcast.class: http_lib.sch 
o/discovery.o discovery.class: service.sch 
o/security.o security.class: param.sch 
o/weblets.o ./weblets.class: xml.sch 
o/param.o param.class: param.sch 
o/svg.o svg.class: xml.sch 
o/param.o ./param.class: param.sch 
o/misc.o misc.class: param.sch 
o/http_remote.o http_remote.class: http_lib.sch 

#bdepend stop
