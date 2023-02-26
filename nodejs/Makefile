#*=====================================================================*/
#*    serrano/prgm/project/hop/hop/nodejs/Makefile                     */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Wed Apr  1 18:45:46 1998                          */
#*    Last change :  Sun Feb 26 18:06:13 2023 (serrano)                */
#*    -------------------------------------------------------------    */
#*    The makefile to build the NODEJS library.                        */
#*=====================================================================*/
.PHONY: do install

do: build
install: install-library install-mo
uninstall: uninstall-mo

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
HOPAPPLIBS = js2scheme hop hopscript

BLIBFLAGS = $(BCFLAGSRTS)

BHOPCFLAGS += --bootstrap-mode

#*---------------------------------------------------------------------*/
#*    Scheme extended objects                                          */
#*---------------------------------------------------------------------*/
_OBJECTS = nodejs require process repl _hop uv _fs _http _crypto \
  _buffer _process_wrap _stream_wrap _tcp_wrap _pipe_wrap _udp_wrap \
  _tty_wrap _timer_wrap _evals _zlib syncg napi
_JS_OBJECTS = console constants util sys path stream events fs \
  punycode assert buffer dgram net timers vm querystring cluster \
  child_process string_decoder _linklist _stream_readable _stream_writable \
  _stream_duplex _stream_transform _stream_passthrough crypto dns domain \
  freelist http https tls tty zlib url os hop hophz \
  node_tick node_stdio node_proc node_timers node_cluster

_MOD_OBJECTS = hop path fs http https 

_C_OBJECTS = _napi

OBJECTS = $(_OBJECTS:%=o/%.o) \
	$(_JS_OBJECTS:%=o/%.o) \
	$(_MOD_OBJECTS:%=o/%.mod.o) \
	$(_C_OBJECTS:%=o/%.o)
EOBJECTS = o/make_lib.o

BGL_CLASSES = $(_OBJECTS:%=$(CLASS_DIR)/%.class)
BGL_ECLASSES = $(CLASS_EDIR)/make_lib.class

SOURCES = $(_OBJECTS:%=%.scm) \
	$(_JS_OBJECTS:%=%.js) \
	$(_MOD_OBJECTS:%=modules/%.mod.js) \
	$(_C_OBJECTS:%=%.c)

INCLUDES = nodejs_debug.sch uv.sch nodejs_types.sch

C_INCLUDES = hop_api.h js_native_api_types.h node_api_types.h \
  js_native_api.h node_api.h

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
	$(MAKE) heap-c
	$(MAKE) $(BACKEND)

native: o heap-c lib-c mod-mo
jvm: heap-jvm lib-jvm

heap-c: .afile
lib-c: .afile heap-c
mod-mo: .afile $(_MOD_OBJECTS:%=modules/mo/%.mod.mo)

modules/mo:
	mkdir -p modules/mo

#*---------------------------------------------------------------------*/
#*    clean                                                            */
#*---------------------------------------------------------------------*/
moclean: 
	$(RM) -rf modules/mo

clean: moclean

#*---------------------------------------------------------------------*/
#*    install-mo                                                       */
#*---------------------------------------------------------------------*/
install-mo: $(DESTDIR)$(HOPLIBDIR)/$(HOPFILDIR)/mo
	for m in $(_MOD_OBJECTS); do \
	   $(INSTALL) modules/mo/$$m.mod.mo $(DESTDIR)$(HOPLIBDIR)/$(HOPFILDIR)/mo; \
	done
	chmod $(MODDIR) $(DESTDIR)$(HOPLIBDIR)/$(HOPFILDIR)/mo
	chmod $(MODFILE) $(DESTDIR)$(HOPLIBDIR)/$(HOPFILDIR)/mo/*.mod.mo

uninstall-mo:
	rm -rf $(DESTDIR)$(HOPLIBDIR)/$(HOPFILDIR)/modules

$(DESTDIR)$(HOPLIBDIR)/$(HOPFILDIR)/mo:
	mkdir -p $@

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
	@ $(call compile3,$(HOPC),$(HFLAGS),$(BCFLAGS),$(BLFLAGS) $(EFLAGS) $(BHOPCFLAGS),-s --js-commonjs-export --js-module-name __nodejs_$* --js-module-path $*,$< -o $@)

o/%.mod.o: modules/%.mod.js
	@ $(call compile3,$(HOPC),$(HFLAGS),$(BCFLAGS),$(BLFLAGS),$(BHOPCFLAGS) -c --js-module-name __nodejs_mod_$* --js-module-path $*,$< -o $@)

o/%.mod.o: modules/%.mod.scm
	@ $(call compile2,$(BIGLOO),$(BCFLAGS),$(BLFLAGS),-c,$< -o $@)

modules/mo/%.mod.mo: modules/%.mod.js modules/mo
	@ $(call compile3,$(HOPC),$(HFLAGS),$(BCFLAGS),$(BLFLAGS),$(BHOPCFLAGS) --js-driver j2s-export-driver -s,$< -o $@)


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

o/http.o: http.js
	@ $(call compile3,$(HOPC),$(HFLAGS),$(BCFLAGS),$(BLFLAGS),$(BHOPCFLAGS) -c --js-module-name __nodejs_http --js-module-path http -Os,$< -o $@)

o/tls.o: tls.js
	@ $(call compile3,$(HOPC),$(HFLAGS),$(BCFLAGS),$(BLFLAGS),$(BHOPCFLAGS) -c --js-module-name __nodejs_tls --js-module-path tls -Os,$< -o $@)

o/buffer.o: buffer.js
	@ $(call compile3,$(HOPC),$(HFLAGS),$(BCFLAGS),$(BLFLAGS),$(BHOPCFLAGS) -c --js-module-name __nodejs_buffer --js-module-path buffer -Os --js-cspecs-get "(imap cmap)" --js-cspecs-assignew "(emap cmap)",$< -o $@)

o/nodejs.mod.o: modules/fs.mod.scm

o/_napi.o: _napi.c 
	@ $(call compile2,$(CCOMP),$(CCFLAGS), -c -g $< -o $@)


#bdepend start (don't edit)
#*---------------------------------------------------------------------*/
#*    Dependencies ...                                                 */
#*---------------------------------------------------------------------*/


#bdepend stop
