#*=====================================================================*/
#*    serrano/prgm/project/hop/hop/runtime/Makefile                    */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Wed Apr  1 18:45:46 1998                          */
#*    Last change :  Mon Sep  9 08:29:20 2024 (serrano)                */
#*    -------------------------------------------------------------    */
#*    The makefile to build the HOP library.                           */
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
TARGETNAME = hop

BLIBFLAGS = $(BCFLAGSRTS)

#*---------------------------------------------------------------------*/
#*    Scheme extended objects                                          */
#*---------------------------------------------------------------------*/
_OBJECTS = param thread misc priv types read module \
           xml xml_types charset dom \
           html_base html_head html_img html_react \
           svg mathml \
           read_js clientc js_comp json \
           css cssmatch hop_rss \
           hop service \
           mime user password cache \
           hop_wiki wiki_syntax wiki_parser wiki_toc hz \
           http_utils \
           http_response http_shoutcast http_proxy http_webdav \
           http_error http_filter \
           hop_inline \
           event websocket color weblets preferences expanders \
           security xdomain zeroconf upnp debug watch base64_vlq

OBJECTS = $(_OBJECTS:%=o/%.o) o/configure.o
EOBJECTS = o/make_lib.o

BGL_CLASSES = $(_OBJECTS:%=$(CLASS_DIR)/%.class) \
              $(CLASS_DIR)/configure.class
BGL_ECLASSES = $(CLASS_EDIR)/make_lib.class

SOURCES = $(_OBJECTS:%=%.scm)

INCLUDES = http_utils.sch expanders.sch \
           xml.sch xml_expd.sch \
           param.sch param_expd.sch \
           hss.sch hss_expd.sch \
           prefs.sch prefs_expd.sch  \
           service.sch service_expd.sch \
           verbose.sch verbose_expd.sch \
           wiki_syntax.sch wiki_parser.sch \
           zeroconf_avahi.sch zeroconf_dummy.sch \
           client-exception.sch sourcemap.sch \
           base64_vlq.sch

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
o/%.o: .afile

#bdepend start (don't edit)
#*---------------------------------------------------------------------*/
#*    Dependencies ...                                                 */
#*---------------------------------------------------------------------*/
zeroconf_avahi.sch: verbose.sch  
o/debug.o debug.class: xml.sch service.sch verbose.sch param.sch         \
      client-exception.sch sourcemap.sch ../scheme2js/base64_vlq.sch 
o/websocket.o websocket.class: http_utils.sch verbose.sch  
o/http_error.o ./http_error.class: xml.sch 
o/http_filter.o http_filter.class: http_utils.sch verbose.sch 
o/websocket.o ./websocket.class: http_utils.sch verbose.sch  
o/wiki_parser.o wiki_parser.class: wiki_syntax.sch wiki_parser.sch 
o/mathml.o ./mathml.class: xml.sch 
o/clientc.o ./clientc.class: xml.sch service.sch 
o/http_proxy.o ./http_proxy.class: http_utils.sch verbose.sch 
o/read.o ./read.class:  
o/http_utils.o http_utils.class: http_utils.sch 
o/security.o ./security.class: param.sch 
o/preferences.o preferences.class: xml.sch service.sch 
o/service.o service.class: service.sch verbose.sch 
o/xdomain.o xdomain.class: service.sch 
o/xml.o ./xml.class: param.sch xml.sch 
o/http_response.o http_response.class: http_utils.sch 
o/http_response.o ./http_response.class: http_utils.sch 
o/expanders.o expanders.class: service_expd.sch prefs_expd.sch           \
      param_expd.sch hss_expd.sch xml_expd.sch verbose_expd.sch          \
      expanders.sch 
o/misc.o ./misc.class: param.sch 
o/http_error.o http_error.class: xml.sch 
o/hop.o hop.class: verbose.sch 
o/css.o ./css.class: xml.sch service.sch hss.sch 
o/mathml.o mathml.class: xml.sch 
o/event.o event.class: xml.sch service.sch verbose.sch param.sch         \
       
o/http_proxy.o http_proxy.class: http_utils.sch verbose.sch 
o/event.o ./event.class: xml.sch service.sch verbose.sch param.sch       \
       
o/user.o user.class: verbose.sch 
o/job.o job.class:  
o/http_webdav.o http_webdav.class: http_utils.sch xml.sch 
o/weblets.o weblets.class: xml.sch verbose.sch 
o/thread.o ./thread.class:  
o/http_utils.o ./http_utils.class: http_utils.sch 
o/html_img.o html_img.class: param.sch xml.sch 
o/html_base.o html_base.class: param.sch xml.sch 
o/svg.o ./svg.class: xml.sch 
o/html_base.o ./html_base.class: param.sch xml.sch 
o/html_head.o html_head.class: xml.sch service.sch 
o/html_head.o ./html_head.class: xml.sch service.sch 
o/upnp.o upnp.class:  
o/hop_inline.o ./hop_inline.class: xml.sch 
o/http_filter.o ./http_filter.class: http_utils.sch verbose.sch 
o/clientc.o clientc.class: xml.sch service.sch 
o/configure.o ./configure.class: configure_noarch.sch 
o/wiki_parser.o ./wiki_parser.class: wiki_syntax.sch wiki_parser.sch 
o/user.o ./user.class: verbose.sch 
o/hop_wiki.o hop_wiki.class: xml.sch 
o/thread.o thread.class:  
o/xml.o xml.class: param.sch xml.sch 
o/preferences.o ./preferences.class: xml.sch service.sch 
o/service.o ./service.class: service.sch verbose.sch 
o/html_img.o ./html_img.class: param.sch xml.sch 
o/xdomain.o ./xdomain.class: service.sch 
o/css.o css.class: xml.sch service.sch hss.sch 
o/zeroconf.o zeroconf.class: service.sch zeroconf_avahi.sch              \
      zeroconf_dummy.sch 
o/hop_inline.o hop_inline.class: xml.sch 
o/hop.o ./hop.class: verbose.sch 
o/hop_rss.o hop_rss.class: xml.sch 
o/http_shoutcast.o http_shoutcast.class: http_utils.sch 
o/read.o read.class:  
o/security.o security.class: param.sch 
o/weblets.o ./weblets.class: xml.sch verbose.sch 
o/param.o param.class: param.sch 
o/svg.o svg.class: xml.sch 
o/param.o ./param.class: param.sch 
o/misc.o misc.class: param.sch 

#bdepend stop
