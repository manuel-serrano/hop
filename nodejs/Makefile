#*=====================================================================*/
#*    serrano/prgm/project/hop/3.0.x/nodejs/Makefile                   */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Wed Apr  1 18:45:46 1998                          */
#*    Last change :  Fri Feb  6 10:57:07 2015 (serrano)                */
#*    -------------------------------------------------------------    */
#*    The makefile to build the NODEJS library.                        */
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
TARGETNAME = nodejs
HOPCLOSELIBS_S = $(BGLCLOSELIBS_S) \
  -ljs2scheme_s-$(HOPRELEASE) -lhopscript_s-$(HOPRELEASE) \
  -lhop_s-$(HOPRELEASE) -lhopscheme_s-$(HOPRELEASE)
HOPCLOSELIBS_U = $(BGLCLOSELIBS_U) \
  -ljs2scheme_u-$(HOPRELEASE) -lhopscript_u-$(HOPRELEASE) \
  -lhop_u-$(HOPRELEASE) -lhopscheme_s-$(HOPRELEASE)
HOPCLOSELIBS_ES = $(BGLCLOSELIBS_ES) \
  -lnodejs_s-$(HOPRELEASE)
HOPCLOSELIBS_EU = $(BGLCLOSELIBS_EU) \
  -lnodejs_u-$(HOPRELEASE)

BLIBFLAGS = $(BCFLAGSRTS)

#*---------------------------------------------------------------------*/
#*    Scheme extended objects                                          */
#*---------------------------------------------------------------------*/
_OBJECTS = nodejs require process repl _hop uv _uv _fs _http _crypto \
  _buffer _process_wrap _stream_wrap _tcp_wrap _pipe_wrap _udp_wrap \
  _tty_wrap _timer_wrap _evals
_JS_OBJECTS = console constants util sys path stream events fs \
  punycode assert buffer dgram net timers vm querystring cluster \
  child_process string_decoder _linklist _stream_readable _stream_writable \
  _stream_duplex _stream_transform _stream_passthrough crypto dns domain \
  freelist http https tls tty zlib url os hop hophz node \
  node_tick node_stdio node_proc node_timers node_cluster

OBJECTS = $(_OBJECTS:%=o/%.o) $(_JS_OBJECTS:%=o/%.o)
EOBJECTS = o/make_lib.o

BGL_CLASSES = $(_OBJECTS:%=$(CLASS_DIR)/%.class)
BGL_ECLASSES = $(CLASS_EDIR)/make_lib.class

SOURCES = $(_OBJECTS:%=%.scm) $(_JS_OBJECTS:%=%.js)

INCLUDES = nodejs_debug.sch nodejs_async.sch

#*---------------------------------------------------------------------*/
#*    Sources                                                          */
#*---------------------------------------------------------------------*/
POPULATION = $(SOURCES) $(INCLUDES) make_lib.scm Makefile

include ../etc/Makefile.library

#*---------------------------------------------------------------------*/
#*    all                                                              */
#*---------------------------------------------------------------------*/
.PHONY: build native jvm

build:
	(LD_LIBRARY_PATH=$(BUILDLIBDIR):$$LD_LIBRARY_PATH; \
         export LD_LIBRARY_PATH; \
         DYLD_LIBRARY_PATH=$(BUILDLIBDIR):$$DYLD_LIBRARY_PATH; \
         export DYLD_LIBRARY_PATH; \
         PATH=$(BUILDBINDIR):$(BUILDLIBDIR):$$PATH; \
         export PATH; \
	 $(MAKE) $(BACKEND))

native: o heap-c lib-c
jvm: heap-jvm lib-jvm

#*---------------------------------------------------------------------*/
#*    Suffixes                                                         */
#*---------------------------------------------------------------------*/
.SUFFIXES: .js

#*---------------------------------------------------------------------*/
#*    The implicit rules                                               */
#*---------------------------------------------------------------------*/
o/js/%.scm: %.js $(HEAP_FILE)
	@ $(call compile3,$(HOPC),$(HFLAGS),$(BCFLAGS),$(BLFLAGS) $(EFLAGS) $(BHOPCFLAGS),-s --js-module-name __nodejs_$* --js-module-path $*,$< -o $@)

$(_JS_OBJECTS:%=o/%.o): $(HEAP_FILE)

#*---------------------------------------------------------------------*/
#*    ude                                                              */
#*---------------------------------------------------------------------*/
.PHONY: ude
ude:
	@ $(MAKE) -f Makefile .afile .etags .jfile
	@ $(MAKE) dep

.afile: $(SOURCES)
	@ $(AFILE) -suffix hop -o .afile $(filter %.scm, $(SOURCES))

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
