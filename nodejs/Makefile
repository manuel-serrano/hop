#*=====================================================================*/
#*    serrano/prgm/project/hop/3.1.x/nodejs/Makefile                   */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Wed Apr  1 18:45:46 1998                          */
#*    Last change :  Sun Nov  5 19:40:38 2017 (serrano)                */
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
  -lhop_s-$(HOPRELEASE)
HOPCLOSELIBS_U = $(BGLCLOSELIBS_U) \
  -ljs2scheme_u-$(HOPRELEASE) -lhopscript_u-$(HOPRELEASE) \
  -lhop_u-$(HOPRELEASE)
HOPCLOSELIBS_ES = $(BGLCLOSELIBS_ES) \
  -lnodejs_s-$(HOPRELEASE)
HOPCLOSELIBS_EU = $(BGLCLOSELIBS_EU) \
  -lnodejs_u-$(HOPRELEASE)

BLIBFLAGS = $(BCFLAGSRTS)

#*---------------------------------------------------------------------*/
#*    Scheme extended objects                                          */
#*---------------------------------------------------------------------*/
_OBJECTS = nodejs require process repl _hop uv _fs _http _crypto \
  _buffer _process_wrap _stream_wrap _tcp_wrap _pipe_wrap _udp_wrap \
  _tty_wrap _timer_wrap _evals _zlib syncg
_JS_OBJECTS = console constants util sys path stream events fs \
  punycode assert buffer dgram net timers vm querystring cluster \
  child_process string_decoder _linklist _stream_readable _stream_writable \
  _stream_duplex _stream_transform _stream_passthrough crypto dns domain \
  freelist http https tls tty zlib url os hop hophz \
  node_tick node_stdio node_proc node_timers node_cluster

OBJECTS = $(_OBJECTS:%=o/%.o) $(_JS_OBJECTS:%=o/%.o)
EOBJECTS = o/make_lib.o

BGL_CLASSES = $(_OBJECTS:%=$(CLASS_DIR)/%.class)
BGL_ECLASSES = $(CLASS_EDIR)/make_lib.class

SOURCES = $(_OBJECTS:%=%.scm) $(_JS_OBJECTS:%=%.js)

INCLUDES = nodejs_debug.sch nodejs_async.sch uv.sch

#*---------------------------------------------------------------------*/
#*    Sources                                                          */
#*---------------------------------------------------------------------*/
POPULATION = $(SOURCES) $(INCLUDES) make_lib.scm Makefile make_fakeuv.scm

include ../etc/Makefile.library

#*---------------------------------------------------------------------*/
#*    all                                                              */
#*---------------------------------------------------------------------*/
.PHONY: build native jvm

build: .afile
	$(MAKE) $(BACKEND)

native: o heap-c lib-c
jvm: heap-jvm lib-jvm

heap-c: .afile
lib-c: .afile

#*---------------------------------------------------------------------*/
#*    uv.sch ...                                                       */
#*---------------------------------------------------------------------*/
uv.sch: uv.scm make_fakeuv.scm
	cat uv.scm | $(BIGLOO) -i make_fakeuv.scm > uv.sch

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

.afile: $(SOURCES) _crypto.sch
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
o/_crypto.o: _crypto.sch

_crypto.sch: $(BUILDETCDIR)/node_crypto_groups.h \
  $(BUILDETCDIR)/node_crypto_groups.c
	$(HOSTCC) $(BUILDETCDIR)/node_crypto_groups.c && ./a.out > $@ \
          || (rm -f _crypto.sch; exit 1) || exit 1
	$(RM) a.out

o/require.o: ../share/hop-boot.js

#bdepend start (don't edit)
#*---------------------------------------------------------------------*/
#*    Dependencies ...                                                 */
#*---------------------------------------------------------------------*/


#bdepend stop
