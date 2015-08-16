#*=====================================================================*/
#*    Author      :  Florian Loitsch                                   */
#*    Creation    :  Wed Mar 15 07:27:50 2006                          */
#*    Last change :  Sun Aug  3 06:43:40 2014 (serrano)                */
#*    Copyright   :  2006-08 Manuel Serrano                            */
#*    -------------------------------------------------------------    */
#*    The Makefile to build the Scheme2JS library.                     */
#*=====================================================================*/
.PHONY: do

do: exec runtime-build

#*---------------------------------------------------------------------*/
#*    Standard Bigloo configuration                                    */
#*---------------------------------------------------------------------*/
include Makefile.configs

#*---------------------------------------------------------------------*/
#*    Compilers, Tools and Destinations                                */
#*---------------------------------------------------------------------*/
# the library name
TARGETNAME = scheme2js
MCO = bglmco -s
EXEC = $(TARGETNAME)

BLIBFLAGS = $(BSCM2JSFLAGS)

#*---------------------------------------------------------------------*/
#*    Scheme extended objects                                          */
#*---------------------------------------------------------------------*/
_BGL_OBJECTS = main scheme2js srfi0 \
        tools trampoline \
        config mutable_strings verbose error \
        module module_resolver module_class module_library \
        expand expanders runtime_expander dsssl_expander dot_expand \
        nodes export walk \
        pobject_conv \
        symbol symbol_table gen_js stdlib \
        letrec_expansion encapsulation node_elimination mark_statements \
	inline deep_clone side use_count nested_closures captured_vars \
        free_vars fun_size transform_util call_check \
        rm_unused_vars \
        tail_rec tail loop_updates var_ref_util \
        constant_propagation constants scope while \
        propagation statements push_set \
        rm_tail_breaks \
	out compile_optimized_call compile_optimized_set \
        compile_optimized_boolify allocate_names template_display \
        push_declarations scm_out \
	pragma dump_node source_map base64_vlq

_BGL_CALLCC_OBJECTS = callcc \
        callcc_anormalform callcc_resume_push callcc_locations \
        callcc_checkpoint callcc_out

_OBJECTS = $(_BGL_OBJECTS)

OBJECTS = $(_OBJECTS:%=o/%.o)
EOBJECTS = o/make_lib.o

BGL_CLASSES = $(_OBJECTS:%=$(CLASS_DIR)/%.class)
BGL_ECLASSES = $(CLASS_EDIR)/make_lib.class

SOURCES = $(_OBJECTS:%=%.scm)

INCLUDES = mapping.sch

#*---------------------------------------------------------------------*/
#*    Sources                                                          */
#*---------------------------------------------------------------------*/
POPULATION = $(SOURCES) $(INCLUDES) make_lib.scm Makefile \
             runtime/exporter.scm \
             runtime/bglcpp.scm \
             runtime/runtime_callcc.js runtime/dsssl.js runtime/dsssl.scm \
             base64_vlq.sch

-include ../etc/Makefile.library

#*---------------------------------------------------------------------*/
#*    all                                                              */
#*---------------------------------------------------------------------*/
.PHONY: build native jvm 

build: $(BACKEND) runtime-build

native: o heap-c lib-c
jvm: heap-jvm lib-jvm

clean: runtime-clean mco-clean

#*---------------------------------------------------------------------*/
#*    scheme2js runtime                                                */
#*---------------------------------------------------------------------*/

SCHEME2JS_RUNTIME = runtime

include $(SCHEME2JS_RUNTIME)/Makefile.include

runtime-install:
	mkdir -p $(DESTDIR)$(SCHEME2JSSHAREDIR) && \
	$(INSTALL) $(SCHEME2JS_RUNTIME)/runtime_u.js \
                $(DESTDIR)$(SCHEME2JSSHAREDIR) && \
        chmod $(MODFILE) $(DESTDIR)$(SCHEME2JSSHAREDIR)/runtime_u.js && \
	$(INSTALL) $(SCHEME2JS_RUNTIME)/runtime_s.js \
                $(DESTDIR)$(SCHEME2JSSHAREDIR) && \
        chmod $(MODFILE) $(DESTDIR)$(SCHEME2JSSHAREDIR)/runtime_s.js && \
	$(INSTALL) $(SCHEME2JS_RUNTIME)/runtime_callcc.js \
                $(DESTDIR)$(SCHEME2JSSHAREDIR) && \
        chmod $(MODFILE) $(DESTDIR)$(SCHEME2JSSHAREDIR)/runtime_callcc.js && \
	$(INSTALL) $(SCHEME2JS_RUNTIME)/runtime_interface.js \
                $(DESTDIR)$(SCHEME2JSSHAREDIR) && \
        chmod $(MODFILE) $(DESTDIR)$(SCHEME2JSSHAREDIR)/runtime_interface.js && \
	$(INSTALL) $(SCHEME2JS_RUNTIME)/runtime_interface_callcc.js \
                $(DESTDIR)$(SCHEME2JSSHAREDIR) && \
        chmod $(MODFILE) $(DESTDIR)$(SCHEME2JSSHAREDIR)/runtime_interface_callcc.js

runtime-uninstall:
	$(RM) -f $(DESTDIR)$(SCHEME2JSSHAREDIR)/runtime_s.js && \
	$(RM) -f $(DESTDIR)$(SCHEME2JSSHAREDIR)/runtime_u.js && \
	$(RM) -f $(DESTDIR)$(SCHEME2JSSHAREDIR)/runtime_callcc.js && \
	$(RM) -f $(DESTDIR)$(SCHEME2JSSHAREDIR)/runtime_interface.js && \
	$(RM) -f $(DESTDIR)$(SCHEME2JSSHAREDIR)/runtime_interface_callcc.js

#*---------------------------------------------------------------------*/
#*    ude                                                              */
#*---------------------------------------------------------------------*/
.PHONY: ude
ude:
	@ $(MAKE) -f Makefile .afile .etags .jfile
	@ $(MAKE) dep

.afile: $(SOURCES)
	@ $(AFILE) -o .afile $(SOURCES)

.jfile: $(SOURCES) 
	@ $(JFILE) -o .jfile \
                   -pbase $(PBASE) $(SOURCES)  make_lib.scm

.etags: $(SOURCES)
	@ $(BTAGS) -o .etags $(SOURCES)

#*---------------------------------------------------------------------*/
#*    Implicit Rules                                                   */
#*---------------------------------------------------------------------*/
MCOS = $(_OBJECTS:%=mco/%.mco)
MCOS2 = $(_OBJECTS:%=mco/%.mco2)

.PRECIOUS: $(MCOS)

mco/%.mco: %.scm
	@mkdir -p mco;
	@if [ ! "$@"2 -nt "$^" ]; then \
	  echo $(MCO) -I . -o $@ $<; \
	  $(MCO) -I . -o $@ $<; \
	fi;
	@touch "$@"2;

mco-clean:
	rm -rf mco

#*---------------------------------------------------------------------*/
#*    runtime/dsssl.js                                                 */
#*---------------------------------------------------------------------*/
runtime/dsssl.js: 
	echo "// generated by ../bin/hopc -j --meta runtime/dsssl.scm -o $@" > $@
	../bin/hopc --meta -j runtime/dsssl.scm >> $@

#*---------------------------------------------------------------------*/
#*    Explicit Dependency
#*---------------------------------------------------------------------*/
$(CLASS_DIR)/stdlib.class o/stdlib.o: $(SCHEME2JS_RUNTIME)/runtime.sch $(SCHEME2JS_RUNTIME)/runtime-callcc.sch
$(CLASS_DIR)/stdlib.class o/stdlib.o: $(SCHEME2JS_RUNTIME)/mod-runtime.sch $(SCHEME2JS_RUNTIME)/mod-runtime-callcc.sch

#bdepend start (don't edit)
#*---------------------------------------------------------------------*/
#*    Dependencies ...                                                 */
#*---------------------------------------------------------------------*/
o/scheme2js.o scheme2js.class: version.sch 
o/scheme2js.o ./scheme2js.class: version.sch 
o/stdlib.o stdlib.class: mapping.sch 
o/stdlib.o stdlib.class: mapping.sch 
o/main.o main.class: version.sch 
o/srfi0.o srfi0.class: version.sch 
o/srfi0.o ./srfi0.class: version.sch 
o/stdlib.o ./stdlib.class: mapping.sch 
o/stdlib.o ./stdlib.class: mapping.sch 

#bdepend stop
