#*=====================================================================*/
#*    Author      :  Florian Loitsch                                   */
#*    Creation    :  Wed Mar 15 07:27:50 2006                          */
#*    Last change :  Fri Dec 18 08:32:02 2009 (serrano)                */
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
        tools \
        config mutable-strings verbose error \
        module module-resolver infotron \
        expand expanders runtime-expander dsssl-expander dot-expand \
        nodes export walk \
        pobject-conv \
        symbol symbol-table gen-js mapping1 mapping2 \
        letrec-expansion encapsulation node-elimination mark-statements \
	inline deep-clone side use-count nested-closures captured-vars \
          free-vars fun-size transform-util call-check \
        rm-unused-vars \
        tail-rec tail loop-updates var-ref-util \
        constant-propagation constants scope while \
        propagation statements push-set \
        rm-tail-breaks \
	out compile-optimized-call compile-optimized-set compile-optimized-boolify \
          allocate-names template-display push-declarations \
        scm-out \
        callcc callcc_anormalform callcc_resume_push callcc_locations callcc_checkpoint \
	trampoline \
	pipe-port \
	js-pp js-lexer js-parser js-nodes js-out \
	pragma

_OBJECTS = $(_BGL_OBJECTS)

OBJECTS = $(_OBJECTS:%=o/%.o)
EOBJECTS = o/make-lib.o

BGL_CLASSES = $(_OBJECTS:%=$(CLASS_DIR)/%.class)
BGL_ECLASSES = $(CLASS_EDIR)/make-lib.class

BGL_DOTNET_OBJ = $(_OBJECTS:%=$(DOTNET_OBJ_DIR)/%.obj)
BGL_DOTNET_EOBJ	= $(DOTNET_OBJ_EDIR)/make-lib.obj

SOURCES = $(_OBJECTS:%=%.scm)

INCLUDES = mapping.sch

#*---------------------------------------------------------------------*/
#*    Sources                                                          */
#*---------------------------------------------------------------------*/
POPULATION = $(SOURCES) $(INCLUDES) make-lib.scm Makefile \
             runtime/exporter.scm \
             runtime/runtime_callcc.js runtime/dsssl.js 

-include ../etc/Makefile.library

#*---------------------------------------------------------------------*/
#*    all                                                              */
#*---------------------------------------------------------------------*/
.PHONY: build native jvm dotnet

build: $(BACKEND) runtime-build

native: heap-c lib-c
jvm: heap-jvm lib-jvm
dotnet: heap-jvm lib-dotnet

clean: runtime-clean mco-clean
install:
uninstall:

#*---------------------------------------------------------------------*/
#*    scheme2js runtime                                                */
#*---------------------------------------------------------------------*/

SCHEME2JS_RUNTIME = runtime

include $(SCHEME2JS_RUNTIME)/Makefile.include

runtime-install:
	mkdir -p $(DESTDIR)$(SCHEME2JSSHAREDIR) && \
	$(INSTALL) $(SCHEME2JS_RUNTIME)/runtime.js \
                $(DESTDIR)$(SCHEME2JSSHAREDIR) && \
	$(INSTALL) $(SCHEME2JS_RUNTIME)/runtime_callcc.js \
                $(DESTDIR)$(SCHEME2JSSHAREDIR) && \
	$(INSTALL) $(SCHEME2JS_RUNTIME)/runtime_interface.js \
                $(DESTDIR)$(SCHEME2JSSHAREDIR) && \
	$(INSTALL) $(SCHEME2JS_RUNTIME)/runtime_interface_callcc.js \
                $(DESTDIR)$(SCHEME2JSSHAREDIR)

runtime-uninstall:
	/bin/rm -f $(DESTDIR)$(SCHEME2JSSHAREDIR)/runtime.js && \
	/bin/rm -f $(DESTDIR)$(SCHEME2JSSHAREDIR)/runtime_callcc.js && \
	/bin/rm -f $(DESTDIR)$(SCHEME2JSSHAREDIR)/runtime_interface.js && \
	/bin/rm -f $(DESTDIR)$(SCHEME2JSSHAREDIR)/runtime_interface_callcc.js

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
                   -pbase $(PBASE) $(SOURCES)  make-lib.scm

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
#*    Explicit Dependency
#*---------------------------------------------------------------------*/
$(CLASS_DIR)/mapping1.class o/mapping1.o: $(SCHEME2JS_RUNTIME)/runtime.sch $(SCHEME2JS_RUNTIME)/runtime-callcc.sch
$(CLASS_DIR)/mapping2.class o/mapping2.o: $(SCHEME2JS_RUNTIME)/mod-runtime.sch $(SCHEME2JS_RUNTIME)/mod-runtime-callcc.sch

#bdepend start (don't edit)
#*---------------------------------------------------------------------*/
#*    Dependencies ...                                                 */
#*---------------------------------------------------------------------*/
o/rm-unused-vars.o classes/indes/inria/scheme2js/rm-unused-vars.class:  \
      mco/verbose.mco mco/use-count.mco mco/walk.mco mco/tools.mco       \
      mco/export.mco mco/nodes.mco 
o/constants.o classes/indes/inria/scheme2js/constants.class:            \
      mco/verbose.mco mco/walk.mco mco/export.mco mco/nodes.mco          \
      mco/tools.mco mco/config.mco 
o/statements.o classes/indes/inria/scheme2js/statements.class:          \
      mco/verbose.mco mco/push-set.mco mco/side.mco                      \
      mco/mark-statements.mco mco/walk.mco mco/config.mco mco/export.mco \
      mco/tools.mco mco/error.mco mco/nodes.mco 
o/walk.o classes/indes/inria/scheme2js/walk.class: mco/export.mco       \
      mco/nodes.mco 
o/compile-optimized-boolify.o classes/indes/inria/scheme2js/compile-optimized-boolify.class: \
      mco/verbose.mco mco/template-display.mco mco/export.mco            \
      mco/nodes.mco mco/tools.mco mco/config.mco 
o/callcc.o classes/indes/inria/scheme2js/callcc.class:                  \
      mco/callcc_resume_push.mco mco/callcc_locations.mco                \
      mco/callcc_checkpoint.mco mco/callcc_anormalform.mco               \
      mco/export.mco mco/tools.mco mco/node-elimination.mco              \
      mco/verbose.mco mco/var-ref-util.mco mco/mark-statements.mco       \
      mco/tail.mco mco/free-vars.mco mco/side.mco mco/walk.mco           \
      mco/nodes.mco mco/config.mco 
o/out.o classes/indes/inria/scheme2js/out.class:                        \
      mco/push-declarations.mco mco/js-pp.mco mco/pipe-port.mco          \
      mco/template-display.mco mco/compile-optimized-set.mco             \
      mco/compile-optimized-boolify.mco mco/compile-optimized-call.mco   \
      mco/callcc.mco mco/allocate-names.mco mco/verbose.mco              \
      mco/mutable-strings.mco mco/gen-js.mco mco/walk.mco mco/export.mco \
      mco/nodes.mco mco/tools.mco mco/config.mco 
o/callcc_checkpoint.o classes/indes/inria/scheme2js/callcc_checkpoint.class: \
      mco/tools.mco mco/verbose.mco mco/callcc_locations.mco             \
      mco/tail.mco mco/walk.mco mco/export.mco mco/nodes.mco             \
      mco/config.mco 
o/push-declarations.o classes/indes/inria/scheme2js/push-declarations.class: \
      mco/verbose.mco mco/tools.mco mco/walk.mco mco/config.mco          \
      mco/export.mco mco/nodes.mco 
o/call-check.o classes/indes/inria/scheme2js/call-check.class:          \
      mco/verbose.mco mco/side.mco mco/walk.mco mco/export.mco           \
      mco/nodes.mco mco/error.mco mco/config.mco 
o/rm-tail-breaks.o classes/indes/inria/scheme2js/rm-tail-breaks.class:  \
      mco/verbose.mco mco/walk.mco mco/export.mco mco/nodes.mco          \
      mco/tools.mco mco/config.mco 
o/tail-rec.o classes/indes/inria/scheme2js/tail-rec.class:              \
      mco/verbose.mco mco/captured-vars.mco mco/loop-updates.mco         \
      mco/tail.mco mco/side.mco mco/transform-util.mco                   \
      mco/var-ref-util.mco mco/symbol.mco mco/walk.mco mco/export.mco    \
      mco/nodes.mco mco/tools.mco mco/config.mco 
o/dot-expand.o classes/indes/inria/scheme2js/dot-expand.class:          \
      mco/expand.mco mco/config.mco mco/mutable-strings.mco              \
      mco/verbose.mco 
o/transform-util.o classes/indes/inria/scheme2js/transform-util.class:  \
      mco/symbol.mco mco/export.mco mco/error.mco mco/nodes.mco 
o/walk.o classes/indes/inria/scheme2js/walk.class: mco/export.mco       \
      mco/nodes.mco 
o/expand.o classes/indes/inria/scheme2js/expand.class: mco/tools.mco    \
      mco/error.mco mco/verbose.mco 
o/trampoline.o classes/indes/inria/scheme2js/trampoline.class:          \
      mco/tools.mco mco/var-ref-util.mco mco/verbose.mco mco/side.mco    \
      mco/tail.mco mco/symbol.mco mco/walk.mco mco/export.mco            \
      mco/nodes.mco mco/config.mco 
o/mapping1.o classes/indes/inria/scheme2js/mapping1.class: mapping.sch  \
      mco/tools.mco mco/export.mco 
o/symbol-table.o classes/indes/inria/scheme2js/symbol-table.class:      \
      mco/tools.mco 
o/encapsulation.o classes/indes/inria/scheme2js/encapsulation.class:    \
      mco/verbose.mco mco/config.mco mco/export.mco mco/nodes.mco        \
      mco/symbol.mco 
o/nested-closures.o classes/indes/inria/scheme2js/nested-closures.class: \
      mco/verbose.mco mco/captured-vars.mco mco/walk.mco mco/export.mco  \
      mco/nodes.mco 
o/compile-optimized-call.o classes/indes/inria/scheme2js/compile-optimized-call.class: \
      mco/verbose.mco mco/export.mco mco/nodes.mco                       \
      mco/template-display.mco mco/tools.mco mco/error.mco               \
      mco/config.mco 
o/infotron.o classes/indes/inria/scheme2js/infotron.class:              \
      mco/config.mco mco/mutable-strings.mco mco/module.mco              \
      mco/error.mco mco/verbose.mco 
o/srfi0.o classes/indes/inria/scheme2js/srfi0.class: mco/expand.mco     \
      mco/error.mco version.sch 
o/symbol.o classes/indes/inria/scheme2js/symbol.class:                  \
      mco/pobject-conv.mco mco/gen-js.mco mco/verbose.mco mco/walk.mco   \
      mco/export.mco mco/nodes.mco mco/config.mco mco/symbol-table.mco   \
      mco/tools.mco mco/mapping2.mco mco/mapping1.mco 
o/allocate-names.o classes/indes/inria/scheme2js/allocate-names.class:  \
      mco/gen-js.mco mco/verbose.mco mco/walk.mco mco/export.mco         \
      mco/nodes.mco mco/tools.mco mco/config.mco 
o/module.o classes/indes/inria/scheme2js/module.class: mco/config.mco   \
      mco/infotron.mco mco/module-resolver.mco mco/export.mco            \
      mco/expand.mco mco/tools.mco mco/srfi0.mco mco/error.mco           \
      mco/gen-js.mco mco/verbose.mco 
o/loop-updates.o classes/indes/inria/scheme2js/loop-updates.class:      \
      mco/verbose.mco mco/var-ref-util.mco mco/walk.mco mco/export.mco   \
      mco/nodes.mco mco/tools.mco mco/config.mco 
o/pobject-conv.o classes/indes/inria/scheme2js/pobject-conv.class:      \
      mco/verbose.mco mco/config.mco mco/export.mco mco/error.mco        \
      mco/nodes.mco 
o/letrec-expansion.o classes/indes/inria/scheme2js/letrec-expansion.class: \
      mco/gen-js.mco mco/verbose.mco mco/walk.mco mco/export.mco         \
      mco/pobject-conv.mco mco/nodes.mco mco/config.mco 
o/runtime-expander.o classes/indes/inria/scheme2js/runtime-expander.class: \
      mco/pobject-conv.mco mco/verbose.mco mco/error.mco mco/config.mco 
o/mapping2.o classes/indes/inria/scheme2js/mapping2.class: mapping.sch  \
      mco/tools.mco mco/export.mco 
o/out.o classes/indes/inria/scheme2js/out.class:                        \
      mco/push-declarations.mco mco/js-pp.mco mco/pipe-port.mco          \
      mco/template-display.mco mco/compile-optimized-set.mco             \
      mco/compile-optimized-boolify.mco mco/compile-optimized-call.mco   \
      mco/callcc.mco mco/allocate-names.mco mco/verbose.mco              \
      mco/mutable-strings.mco mco/gen-js.mco mco/walk.mco mco/export.mco \
      mco/nodes.mco mco/tools.mco mco/config.mco 
o/push-set.o classes/indes/inria/scheme2js/push-set.class:              \
      mco/verbose.mco mco/walk.mco mco/export.mco mco/nodes.mco 
o/srfi0.o classes/indes/inria/scheme2js/srfi0.class: mco/expand.mco     \
      mco/error.mco version.sch 
o/js-parser.o classes/indes/inria/scheme2js/js-parser.class:            \
      mco/js-lexer.mco mco/js-nodes.mco 
o/var-ref-util.o classes/indes/inria/scheme2js/var-ref-util.class:      \
      mco/export.mco mco/nodes.mco mco/tools.mco mco/config.mco 
o/while.o classes/indes/inria/scheme2js/while.class: mco/verbose.mco    \
      mco/loop-updates.mco mco/symbol.mco mco/walk.mco mco/export.mco    \
      mco/nodes.mco mco/tools.mco mco/config.mco 
o/nodes.o classes/indes/inria/scheme2js/nodes.class: mco/export.mco     \
      mco/verbose.mco 
o/expand.o classes/indes/inria/scheme2js/expand.class: mco/tools.mco    \
      mco/error.mco mco/verbose.mco 
o/rm-tail-breaks.o classes/indes/inria/scheme2js/rm-tail-breaks.class:  \
      mco/verbose.mco mco/walk.mco mco/export.mco mco/nodes.mco          \
      mco/tools.mco mco/config.mco 
o/export.o classes/indes/inria/scheme2js/export.class: mco/error.mco    \
      mco/gen-js.mco 
o/expanders.o classes/indes/inria/scheme2js/expanders.class:            \
      mco/pobject-conv.mco mco/error.mco mco/config.mco mco/expand.mco 
o/letrec-expansion.o classes/indes/inria/scheme2js/letrec-expansion.class: \
      mco/gen-js.mco mco/verbose.mco mco/walk.mco mco/export.mco         \
      mco/pobject-conv.mco mco/nodes.mco mco/config.mco 
o/mark-statements.o classes/indes/inria/scheme2js/mark-statements.class: \
      mco/verbose.mco mco/config.mco mco/walk.mco mco/export.mco         \
      mco/nodes.mco 
o/runtime-expander.o classes/indes/inria/scheme2js/runtime-expander.class: \
      mco/pobject-conv.mco mco/verbose.mco mco/error.mco mco/config.mco 
o/scm-out.o classes/indes/inria/scheme2js/scm-out.class: mco/walk.mco   \
      mco/tools.mco mco/export.mco mco/nodes.mco 
o/transform-util.o classes/indes/inria/scheme2js/transform-util.class:  \
      mco/symbol.mco mco/export.mco mco/error.mco mco/nodes.mco 
o/inline.o classes/indes/inria/scheme2js/inline.class: mco/verbose.mco  \
      mco/transform-util.mco mco/fun-size.mco mco/nested-closures.mco    \
      mco/use-count.mco mco/side.mco mco/deep-clone.mco mco/walk.mco     \
      mco/export.mco mco/nodes.mco mco/tools.mco mco/config.mco 
o/push-declarations.o classes/indes/inria/scheme2js/push-declarations.class: \
      mco/verbose.mco mco/tools.mco mco/walk.mco mco/config.mco          \
      mco/export.mco mco/nodes.mco 
o/js-pp.o classes/indes/inria/scheme2js/js-pp.class: mco/js-out.mco     \
      mco/js-nodes.mco mco/js-parser.mco 
o/scm-out.o classes/indes/inria/scheme2js/scm-out.class: mco/walk.mco   \
      mco/tools.mco mco/export.mco mco/nodes.mco 
o/use-count.o classes/indes/inria/scheme2js/use-count.class:            \
      mco/verbose.mco mco/walk.mco mco/export.mco mco/nodes.mco 
o/js-out.o classes/indes/inria/scheme2js/js-out.class: mco/js-lexer.mco \
      mco/js-nodes.mco 
o/nodes.o classes/indes/inria/scheme2js/nodes.class: mco/export.mco     \
      mco/verbose.mco 
o/captured-vars.o classes/indes/inria/scheme2js/captured-vars.class:    \
      mco/verbose.mco mco/side.mco mco/free-vars.mco mco/walk.mco        \
      mco/export.mco mco/nodes.mco 
o/symbol.o classes/indes/inria/scheme2js/symbol.class:                  \
      mco/pobject-conv.mco mco/gen-js.mco mco/verbose.mco mco/walk.mco   \
      mco/export.mco mco/nodes.mco mco/config.mco mco/symbol-table.mco   \
      mco/tools.mco mco/mapping2.mco mco/mapping1.mco 
o/module.o classes/indes/inria/scheme2js/module.class: mco/config.mco   \
      mco/infotron.mco mco/module-resolver.mco mco/export.mco            \
      mco/expand.mco mco/tools.mco mco/srfi0.mco mco/error.mco           \
      mco/gen-js.mco mco/verbose.mco 
o/while.o classes/indes/inria/scheme2js/while.class: mco/verbose.mco    \
      mco/loop-updates.mco mco/symbol.mco mco/walk.mco mco/export.mco    \
      mco/nodes.mco mco/tools.mco mco/config.mco 
o/deep-clone.o classes/indes/inria/scheme2js/deep-clone.class:          \
      mco/verbose.mco mco/walk.mco mco/tools.mco mco/export.mco          \
      mco/nodes.mco 
o/free-vars.o classes/indes/inria/scheme2js/free-vars.class:            \
      mco/verbose.mco mco/walk.mco mco/tools.mco mco/export.mco          \
      mco/nodes.mco 
o/statements.o classes/indes/inria/scheme2js/statements.class:          \
      mco/verbose.mco mco/push-set.mco mco/side.mco                      \
      mco/mark-statements.mco mco/walk.mco mco/config.mco mco/export.mco \
      mco/tools.mco mco/error.mco mco/nodes.mco 
o/js-parser.o classes/indes/inria/scheme2js/js-parser.class:            \
      mco/js-lexer.mco mco/js-nodes.mco 
o/js-pp.o classes/indes/inria/scheme2js/js-pp.class: mco/js-out.mco     \
      mco/js-nodes.mco mco/js-parser.mco 
o/fun-size.o classes/indes/inria/scheme2js/fun-size.class:              \
      mco/verbose.mco mco/walk.mco mco/export.mco mco/nodes.mco 
o/mapping1.o classes/indes/inria/scheme2js/mapping1.class: mapping.sch  \
      mco/tools.mco mco/export.mco 
o/node-elimination.o classes/indes/inria/scheme2js/node-elimination.class: \
      mco/mark-statements.mco mco/verbose.mco mco/walk.mco               \
      mco/export.mco mco/nodes.mco 
o/scope.o classes/indes/inria/scheme2js/scope.class:                    \
      mco/captured-vars.mco mco/free-vars.mco mco/verbose.mco            \
      mco/walk.mco mco/export.mco mco/nodes.mco mco/tools.mco            \
      mco/config.mco 
o/expanders.o classes/indes/inria/scheme2js/expanders.class:            \
      mco/pobject-conv.mco mco/error.mco mco/config.mco mco/expand.mco 
o/tail.o classes/indes/inria/scheme2js/tail.class: mco/verbose.mco      \
      mco/walk.mco mco/export.mco mco/nodes.mco 
o/template-display.o classes/indes/inria/scheme2js/template-display.class: \
      mco/tools.mco 
o/call-check.o classes/indes/inria/scheme2js/call-check.class:          \
      mco/verbose.mco mco/side.mco mco/walk.mco mco/export.mco           \
      mco/nodes.mco mco/error.mco mco/config.mco 
o/infotron.o classes/indes/inria/scheme2js/infotron.class:              \
      mco/config.mco mco/mutable-strings.mco mco/module.mco              \
      mco/error.mco mco/verbose.mco 
o/compile-optimized-set.o classes/indes/inria/scheme2js/compile-optimized-set.class: \
      mco/export.mco mco/allocate-names.mco mco/nodes.mco                \
      mco/template-display.mco mco/tools.mco mco/config.mco 
o/dot-expand.o classes/indes/inria/scheme2js/dot-expand.class:          \
      mco/expand.mco mco/config.mco mco/mutable-strings.mco              \
      mco/verbose.mco 
o/use-count.o classes/indes/inria/scheme2js/use-count.class:            \
      mco/verbose.mco mco/walk.mco mco/export.mco mco/nodes.mco 
o/export.o classes/indes/inria/scheme2js/export.class: mco/error.mco    \
      mco/gen-js.mco 
o/callcc_resume_push.o classes/indes/inria/scheme2js/callcc_resume_push.class: \
      mco/tools.mco mco/verbose.mco mco/mark-statements.mco mco/tail.mco \
      mco/walk.mco mco/export.mco mco/nodes.mco mco/config.mco 
o/propagation.o classes/indes/inria/scheme2js/propagation.class:        \
      mco/mutable-strings.mco mco/verbose.mco mco/side.mco               \
      mco/free-vars.mco mco/var-ref-util.mco mco/walk.mco mco/export.mco \
      mco/nodes.mco mco/tools.mco mco/config.mco 
o/constant-propagation.o classes/indes/inria/scheme2js/constant-propagation.class: \
      mco/verbose.mco mco/use-count.mco mco/side.mco                     \
      mco/var-ref-util.mco mco/walk.mco mco/export.mco mco/nodes.mco     \
      mco/config.mco 
o/pobject-conv.o classes/indes/inria/scheme2js/pobject-conv.class:      \
      mco/verbose.mco mco/config.mco mco/export.mco mco/error.mco        \
      mco/nodes.mco 
o/inline.o classes/indes/inria/scheme2js/inline.class: mco/verbose.mco  \
      mco/transform-util.mco mco/fun-size.mco mco/nested-closures.mco    \
      mco/use-count.mco mco/side.mco mco/deep-clone.mco mco/walk.mco     \
      mco/export.mco mco/nodes.mco mco/tools.mco mco/config.mco 
o/scope.o classes/indes/inria/scheme2js/scope.class:                    \
      mco/captured-vars.mco mco/free-vars.mco mco/verbose.mco            \
      mco/walk.mco mco/export.mco mco/nodes.mco mco/tools.mco            \
      mco/config.mco 
o/callcc_resume_push.o classes/indes/inria/scheme2js/callcc_resume_push.class: \
      mco/tools.mco mco/verbose.mco mco/mark-statements.mco mco/tail.mco \
      mco/walk.mco mco/export.mco mco/nodes.mco mco/config.mco 
o/free-vars.o classes/indes/inria/scheme2js/free-vars.class:            \
      mco/verbose.mco mco/walk.mco mco/tools.mco mco/export.mco          \
      mco/nodes.mco 
o/js-out.o classes/indes/inria/scheme2js/js-out.class: mco/js-lexer.mco \
      mco/js-nodes.mco 
o/tail.o classes/indes/inria/scheme2js/tail.class: mco/verbose.mco      \
      mco/walk.mco mco/export.mco mco/nodes.mco 
o/node-elimination.o classes/indes/inria/scheme2js/node-elimination.class: \
      mco/mark-statements.mco mco/verbose.mco mco/walk.mco               \
      mco/export.mco mco/nodes.mco 
o/compile-optimized-set.o classes/indes/inria/scheme2js/compile-optimized-set.class: \
      mco/export.mco mco/allocate-names.mco mco/nodes.mco                \
      mco/template-display.mco mco/tools.mco mco/config.mco 
o/trampoline.o classes/indes/inria/scheme2js/trampoline.class:          \
      mco/tools.mco mco/var-ref-util.mco mco/verbose.mco mco/side.mco    \
      mco/tail.mco mco/symbol.mco mco/walk.mco mco/export.mco            \
      mco/nodes.mco mco/config.mco 
o/template-display.o classes/indes/inria/scheme2js/template-display.class: \
      mco/tools.mco 
o/verbose.o classes/indes/inria/scheme2js/verbose.class: mco/config.mco 
o/constant-propagation.o classes/indes/inria/scheme2js/constant-propagation.class: \
      mco/verbose.mco mco/use-count.mco mco/side.mco                     \
      mco/var-ref-util.mco mco/walk.mco mco/export.mco mco/nodes.mco     \
      mco/config.mco 
o/callcc_checkpoint.o classes/indes/inria/scheme2js/callcc_checkpoint.class: \
      mco/tools.mco mco/verbose.mco mco/callcc_locations.mco             \
      mco/tail.mco mco/walk.mco mco/export.mco mco/nodes.mco             \
      mco/config.mco 
o/rm-unused-vars.o classes/indes/inria/scheme2js/rm-unused-vars.class:  \
      mco/verbose.mco mco/use-count.mco mco/walk.mco mco/tools.mco       \
      mco/export.mco mco/nodes.mco 
o/nested-closures.o classes/indes/inria/scheme2js/nested-closures.class: \
      mco/verbose.mco mco/captured-vars.mco mco/walk.mco mco/export.mco  \
      mco/nodes.mco 
o/verbose.o classes/indes/inria/scheme2js/verbose.class: mco/config.mco 
o/tail-rec.o classes/indes/inria/scheme2js/tail-rec.class:              \
      mco/verbose.mco mco/captured-vars.mco mco/loop-updates.mco         \
      mco/tail.mco mco/side.mco mco/transform-util.mco                   \
      mco/var-ref-util.mco mco/symbol.mco mco/walk.mco mco/export.mco    \
      mco/nodes.mco mco/tools.mco mco/config.mco 
o/main.o classes/indes/inria/scheme2js/main.class: version.sch          \
      mco/config.mco mco/scheme2js.mco 
o/symbol-table.o classes/indes/inria/scheme2js/symbol-table.class:      \
      mco/tools.mco 
o/encapsulation.o classes/indes/inria/scheme2js/encapsulation.class:    \
      mco/verbose.mco mco/config.mco mco/export.mco mco/nodes.mco        \
      mco/symbol.mco 
o/error.o classes/indes/inria/scheme2js/error.class: mco/nodes.mco      \
      mco/export.mco 
o/scheme2js.o classes/indes/inria/scheme2js/scheme2js.class:            \
      mco/trampoline.mco mco/callcc.mco mco/verbose.mco mco/scm-out.mco  \
      mco/out.mco mco/rm-tail-breaks.mco mco/statements.mco              \
      mco/propagation.mco mco/while.mco mco/scope.mco mco/constants.mco  \
      mco/constant-propagation.mco mco/tail-rec.mco                      \
      mco/rm-unused-vars.mco mco/call-check.mco mco/inline.mco           \
      mco/node-elimination.mco mco/encapsulation.mco                     \
      mco/letrec-expansion.mco mco/symbol.mco mco/pobject-conv.mco       \
      mco/dot-expand.mco mco/runtime-expander.mco mco/expanders.mco      \
      mco/expand.mco mco/module.mco mco/export.mco mco/srfi0.mco         \
      mco/nodes.mco mco/config.mco version.sch 
o/callcc_anormalform.o classes/indes/inria/scheme2js/callcc_anormalform.class: \
      mco/tools.mco mco/side.mco mco/verbose.mco                         \
      mco/callcc_locations.mco mco/walk.mco mco/export.mco mco/nodes.mco \
      mco/config.mco 
o/callcc.o classes/indes/inria/scheme2js/callcc.class:                  \
      mco/callcc_resume_push.mco mco/callcc_locations.mco                \
      mco/callcc_checkpoint.mco mco/callcc_anormalform.mco               \
      mco/export.mco mco/tools.mco mco/node-elimination.mco              \
      mco/verbose.mco mco/var-ref-util.mco mco/mark-statements.mco       \
      mco/tail.mco mco/free-vars.mco mco/side.mco mco/walk.mco           \
      mco/nodes.mco mco/config.mco 
o/callcc_locations.o classes/indes/inria/scheme2js/callcc_locations.class: \
      mco/tools.mco mco/verbose.mco mco/var-ref-util.mco                 \
      mco/free-vars.mco mco/side.mco mco/export.mco mco/walk.mco         \
      mco/nodes.mco mco/config.mco 
o/loop-updates.o classes/indes/inria/scheme2js/loop-updates.class:      \
      mco/verbose.mco mco/var-ref-util.mco mco/walk.mco mco/export.mco   \
      mco/nodes.mco mco/tools.mco mco/config.mco 
o/callcc_anormalform.o classes/indes/inria/scheme2js/callcc_anormalform.class: \
      mco/tools.mco mco/side.mco mco/verbose.mco                         \
      mco/callcc_locations.mco mco/walk.mco mco/export.mco mco/nodes.mco \
      mco/config.mco 
o/side.o classes/indes/inria/scheme2js/side.class: mco/verbose.mco      \
      mco/walk.mco mco/export.mco mco/nodes.mco mco/error.mco            \
      mco/config.mco 
o/constants.o classes/indes/inria/scheme2js/constants.class:            \
      mco/verbose.mco mco/walk.mco mco/export.mco mco/nodes.mco          \
      mco/tools.mco mco/config.mco 
o/compile-optimized-boolify.o classes/indes/inria/scheme2js/compile-optimized-boolify.class: \
      mco/verbose.mco mco/template-display.mco mco/export.mco            \
      mco/nodes.mco mco/tools.mco mco/config.mco 
o/fun-size.o classes/indes/inria/scheme2js/fun-size.class:              \
      mco/verbose.mco mco/walk.mco mco/export.mco mco/nodes.mco 
o/captured-vars.o classes/indes/inria/scheme2js/captured-vars.class:    \
      mco/verbose.mco mco/side.mco mco/free-vars.mco mco/walk.mco        \
      mco/export.mco mco/nodes.mco 
o/error.o classes/indes/inria/scheme2js/error.class: mco/nodes.mco      \
      mco/export.mco 
o/propagation.o classes/indes/inria/scheme2js/propagation.class:        \
      mco/mutable-strings.mco mco/verbose.mco mco/side.mco               \
      mco/free-vars.mco mco/var-ref-util.mco mco/walk.mco mco/export.mco \
      mco/nodes.mco mco/tools.mco mco/config.mco 
o/mapping2.o classes/indes/inria/scheme2js/mapping2.class: mapping.sch  \
      mco/tools.mco mco/export.mco 
o/mark-statements.o classes/indes/inria/scheme2js/mark-statements.class: \
      mco/verbose.mco mco/config.mco mco/walk.mco mco/export.mco         \
      mco/nodes.mco 
o/allocate-names.o classes/indes/inria/scheme2js/allocate-names.class:  \
      mco/gen-js.mco mco/verbose.mco mco/walk.mco mco/export.mco         \
      mco/nodes.mco mco/tools.mco mco/config.mco 
o/push-set.o classes/indes/inria/scheme2js/push-set.class:              \
      mco/verbose.mco mco/walk.mco mco/export.mco mco/nodes.mco 
o/var-ref-util.o classes/indes/inria/scheme2js/var-ref-util.class:      \
      mco/export.mco mco/nodes.mco mco/tools.mco mco/config.mco 
o/side.o classes/indes/inria/scheme2js/side.class: mco/verbose.mco      \
      mco/walk.mco mco/export.mco mco/nodes.mco mco/error.mco            \
      mco/config.mco 
o/deep-clone.o classes/indes/inria/scheme2js/deep-clone.class:          \
      mco/verbose.mco mco/walk.mco mco/tools.mco mco/export.mco          \
      mco/nodes.mco 
o/scheme2js.o classes/indes/inria/scheme2js/scheme2js.class:            \
      mco/trampoline.mco mco/callcc.mco mco/verbose.mco mco/scm-out.mco  \
      mco/out.mco mco/rm-tail-breaks.mco mco/statements.mco              \
      mco/propagation.mco mco/while.mco mco/scope.mco mco/constants.mco  \
      mco/constant-propagation.mco mco/tail-rec.mco                      \
      mco/rm-unused-vars.mco mco/call-check.mco mco/inline.mco           \
      mco/node-elimination.mco mco/encapsulation.mco                     \
      mco/letrec-expansion.mco mco/symbol.mco mco/pobject-conv.mco       \
      mco/dot-expand.mco mco/runtime-expander.mco mco/expanders.mco      \
      mco/expand.mco mco/module.mco mco/export.mco mco/srfi0.mco         \
      mco/nodes.mco mco/config.mco version.sch 
o/callcc_locations.o classes/indes/inria/scheme2js/callcc_locations.class: \
      mco/tools.mco mco/verbose.mco mco/var-ref-util.mco                 \
      mco/free-vars.mco mco/side.mco mco/export.mco mco/walk.mco         \
      mco/nodes.mco mco/config.mco 
o/compile-optimized-call.o classes/indes/inria/scheme2js/compile-optimized-call.class: \
      mco/verbose.mco mco/export.mco mco/nodes.mco                       \
      mco/template-display.mco mco/tools.mco mco/error.mco               \
      mco/config.mco 

#bdepend stop
