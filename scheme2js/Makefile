#*=====================================================================*/
#*    serrano/prgm/project/hop/1.9.x/scheme2js/Makefile                */
#*    -------------------------------------------------------------    */
#*    Author      :  Florian Loitsch                                   */
#*    Creation    :  Wed Mar 15 07:27:50 2006                          */
#*    Last change :  Wed Apr  9 10:09:47 2008 (serrano)                */
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
_BGL_OBJECTS = compile constants expanders letrec_expansion scope \
	gen-js inline liveness node-elimination \
	nodes pobject-conv protobject scheme2js side statements symbol \
	tail-rec tail transform-util traverse var expand verbose \
	dot-expand allocate-names config locals use-count \
	nested-closures fun-size rm-unused-vars captured-vars loop-updates \
	free-vars propagation mark-statements while trampoline \
	callcc callcc_checkpoint callcc_anormalform callcc_locations callcc_resume_push \
        locations encapsulation loop-hoist check deep-clone \
	var-ref-util compile-optimized-call compile-optimized-boolify \
	compile-optimized-set var-elimination constant-propagation \
	runtime-expander main mapping1 mapping2 \
	module mutable-strings infotron

_OBJECTS = $(_BGL_OBJECTS)

OBJECTS = $(_OBJECTS:%=o/%.o)
EOBJECTS = o/make-lib.o

BGL_CLASSES = $(_OBJECTS:%=$(CLASS_DIR)/%.class)
BGL_ECLASSES = $(CLASS_EDIR)/make-lib.class

BGL_DOTNET_OBJ = $(_OBJECTS:%=$(DOTNET_OBJ_DIR)/%.obj)
BGL_DOTNET_EOBJ	= $(DOTNET_OBJ_EDIR)/make-lib.obj

SOURCES = $(_OBJECTS:%=%.scm)

INCLUDES = nodes.sch protobject.sch tools.sch mapping.sch

#*---------------------------------------------------------------------*/
#*    Sources                                                          */
#*---------------------------------------------------------------------*/
POPULATION = $(SOURCES) $(INCLUDES) make-lib.scm Makefile \
             runtime/exporter.scm runtime/runtime_callcc.js \
             Makefile.configs \
	     runtime/IO.js \
             runtime/Makefile.include \
             runtime/immutable.js \
             runtime/interface.scm \
             runtime/meta.scm \
             runtime/multi-file-port.scm \
             runtime/mutable.js \
             runtime/runtime_part.js \
             runtime/string.scm

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
	install -m a+r $(SCHEME2JS_RUNTIME)/runtime.js \
                $(DESTDIR)$(SCHEME2JSSHAREDIR) && \
	install -m a+r $(SCHEME2JS_RUNTIME)/runtime_callcc.js \
                $(DESTDIR)$(SCHEME2JSSHAREDIR) && \
	install -m a+r $(SCHEME2JS_RUNTIME)/runtime_interface.js \
                $(DESTDIR)$(SCHEME2JSSHAREDIR) && \
	install -m a+r $(SCHEME2JS_RUNTIME)/runtime_interface_callcc.js \
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

.PRECIOUS: $(MCOS)

mco/%.mco: %.scm
	@mkdir -p mco
	$(MCO) -I . -o $@ $<

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
o/var.o classes/mimosa/inria/scheme2js/var.class: mco/nodes.mco          \
      mco/protobject.mco nodes.sch protobject.sch 
o/rm-unused-vars.o classes/mimosa/inria/scheme2js/rm-unused-vars.class:  \
      mco/verbose.mco mco/use-count.mco mco/var.mco mco/nodes.mco        \
      mco/protobject.mco tools.sch nodes.sch protobject.sch 
o/locals.o classes/mimosa/inria/scheme2js/locals.class: mco/verbose.mco  \
      mco/nodes.mco mco/protobject.mco tools.sch nodes.sch               \
      protobject.sch 
o/statements.o classes/mimosa/inria/scheme2js/statements.class:          \
      mco/verbose.mco mco/var.mco mco/nodes.mco mco/mark-statements.mco  \
      mco/protobject.mco tools.sch nodes.sch protobject.sch 
o/constants.o classes/mimosa/inria/scheme2js/constants.class:            \
      mco/verbose.mco mco/var.mco mco/mark-statements.mco mco/nodes.mco  \
      mco/config.mco mco/protobject.mco tools.sch nodes.sch              \
      protobject.sch 
o/compile-optimized-boolify.o classes/mimosa/inria/scheme2js/compile-optimized-boolify.class: \
      mco/verbose.mco mco/var.mco mco/nodes.mco mco/config.mco           \
      mco/protobject.mco tools.sch nodes.sch protobject.sch 
o/protobject.o classes/mimosa/inria/scheme2js/protobject.class:          \
      tools.sch protobject.sch 
o/callcc.o classes/mimosa/inria/scheme2js/callcc.class:                  \
      mco/callcc_resume_push.mco mco/callcc_locations.mco                \
      mco/callcc_checkpoint.mco mco/callcc_anormalform.mco               \
      mco/node-elimination.mco mco/verbose.mco mco/var-ref-util.mco      \
      mco/var.mco mco/mark-statements.mco mco/locals.mco mco/tail.mco    \
      mco/free-vars.mco mco/side.mco mco/nodes.mco mco/config.mco        \
      mco/protobject.mco tools.sch nodes.sch protobject.sch 
o/traverse.o classes/mimosa/inria/scheme2js/traverse.class:              \
      mco/verbose.mco mco/nodes.mco mco/protobject.mco nodes.sch         \
      protobject.sch 
o/callcc_checkpoint.o classes/mimosa/inria/scheme2js/callcc_checkpoint.class: \
      mco/verbose.mco mco/callcc_locations.mco mco/tail.mco              \
      mco/nodes.mco mco/config.mco mco/protobject.mco tools.sch          \
      nodes.sch protobject.sch 
o/dot-expand.o classes/mimosa/inria/scheme2js/dot-expand.class:          \
      mco/expand.mco mco/config.mco mco/mutable-strings.mco              \
      mco/verbose.mco 
o/tail-rec.o classes/mimosa/inria/scheme2js/tail-rec.class:              \
      mco/verbose.mco mco/captured-vars.mco mco/loop-updates.mco         \
      mco/tail.mco mco/side.mco mco/transform-util.mco                   \
      mco/var-ref-util.mco mco/var.mco mco/symbol.mco mco/nodes.mco      \
      mco/config.mco mco/protobject.mco tools.sch nodes.sch              \
      protobject.sch 
o/transform-util.o classes/mimosa/inria/scheme2js/transform-util.class:  \
      mco/var.mco mco/symbol.mco mco/nodes.mco mco/protobject.mco        \
      nodes.sch protobject.sch 
o/expand.o classes/mimosa/inria/scheme2js/expand.class: tools.sch        \
      mco/verbose.mco 
o/var-elimination.o classes/mimosa/inria/scheme2js/var-elimination.class: \
      mco/verbose.mco mco/use-count.mco mco/side.mco mco/var.mco         \
      mco/nodes.mco mco/config.mco mco/var-ref-util.mco                  \
      mco/protobject.mco tools.sch nodes.sch protobject.sch 
o/trampoline.o classes/mimosa/inria/scheme2js/trampoline.class:          \
      mco/var-ref-util.mco mco/verbose.mco mco/side.mco mco/tail.mco     \
      mco/var.mco mco/symbol.mco mco/nodes.mco mco/config.mco            \
      mco/protobject.mco tools.sch nodes.sch protobject.sch 
o/mapping1.o classes/mimosa/inria/scheme2js/mapping1.class: mapping.sch 
o/liveness.o classes/mimosa/inria/scheme2js/liveness.class:              \
      mco/verbose.mco mco/nodes.mco mco/captured-vars.mco                \
      mco/protobject.mco nodes.sch protobject.sch 
o/encapsulation.o classes/mimosa/inria/scheme2js/encapsulation.class:    \
      mco/verbose.mco mco/config.mco mco/var.mco mco/nodes.mco           \
      mco/symbol.mco mco/protobject.mco tools.sch nodes.sch              \
      protobject.sch 
o/nested-closures.o classes/mimosa/inria/scheme2js/nested-closures.class: \
      mco/verbose.mco mco/captured-vars.mco mco/free-vars.mco            \
      mco/locals.mco mco/nodes.mco mco/protobject.mco tools.sch          \
      nodes.sch protobject.sch 
o/infotron.o classes/mimosa/inria/scheme2js/infotron.class:              \
      mco/config.mco mco/mutable-strings.mco mco/module.mco              \
      mco/verbose.mco 
o/compile-optimized-call.o classes/mimosa/inria/scheme2js/compile-optimized-call.class: \
      mco/verbose.mco mco/var.mco mco/nodes.mco mco/config.mco           \
      mco/protobject.mco tools.sch nodes.sch protobject.sch 
o/module.o classes/mimosa/inria/scheme2js/module.class: mco/config.mco   \
      mco/infotron.mco mco/verbose.mco 
o/allocate-names.o classes/mimosa/inria/scheme2js/allocate-names.class:  \
      mco/gen-js.mco mco/verbose.mco mco/var.mco mco/nodes.mco           \
      mco/config.mco mco/free-vars.mco mco/protobject.mco tools.sch      \
      nodes.sch protobject.sch 
o/symbol.o classes/mimosa/inria/scheme2js/symbol.class: mco/gen-js.mco   \
      mco/verbose.mco mco/var.mco mco/nodes.mco mco/config.mco           \
      mco/mapping2.mco mco/mapping1.mco mco/protobject.mco tools.sch     \
      nodes.sch protobject.sch 
o/letrec_expansion.o classes/mimosa/inria/scheme2js/letrec_expansion.class: \
      mco/gen-js.mco mco/verbose.mco mco/var.mco mco/nodes.mco           \
      mco/config.mco mco/protobject.mco nodes.sch protobject.sch 
o/loop-updates.o classes/mimosa/inria/scheme2js/loop-updates.class:      \
      mco/verbose.mco mco/config.mco mco/var.mco mco/var-ref-util.mco    \
      mco/nodes.mco mco/protobject.mco tools.sch nodes.sch               \
      protobject.sch 
o/pobject-conv.o classes/mimosa/inria/scheme2js/pobject-conv.class:      \
      mco/verbose.mco mco/protobject.mco mco/config.mco mco/nodes.mco    \
      nodes.sch protobject.sch 
o/locations.o classes/mimosa/inria/scheme2js/locations.class:            \
      mco/nodes.mco mco/verbose.mco mco/config.mco mco/protobject.mco    \
      tools.sch nodes.sch protobject.sch 
o/runtime-expander.o classes/mimosa/inria/scheme2js/runtime-expander.class: \
      mco/pobject-conv.mco mco/verbose.mco mco/config.mco 
o/mapping2.o classes/mimosa/inria/scheme2js/mapping2.class: mapping.sch 
o/loop-hoist.o classes/mimosa/inria/scheme2js/loop-hoist.class:          \
      mco/verbose.mco mco/nodes.mco mco/captured-vars.mco mco/side.mco   \
      mco/config.mco mco/protobject.mco tools.sch nodes.sch              \
      protobject.sch 
o/var-ref-util.o classes/mimosa/inria/scheme2js/var-ref-util.class:      \
      mco/nodes.mco mco/config.mco mco/protobject.mco tools.sch          \
      nodes.sch protobject.sch 
o/while.o classes/mimosa/inria/scheme2js/while.class: mco/verbose.mco    \
      mco/loop-updates.mco mco/tail.mco mco/symbol.mco mco/var.mco       \
      mco/nodes.mco mco/config.mco mco/mark-statements.mco               \
      mco/protobject.mco tools.sch nodes.sch protobject.sch 
o/nodes.o classes/mimosa/inria/scheme2js/nodes.class: mco/verbose.mco    \
      mco/protobject.mco protobject.sch 
o/letrec_expansion.o classes/mimosa/inria/scheme2js/letrec_expansion.class: \
      mco/gen-js.mco mco/verbose.mco mco/var.mco mco/nodes.mco           \
      mco/config.mco mco/protobject.mco nodes.sch protobject.sch 
o/expand.o classes/mimosa/inria/scheme2js/expand.class: tools.sch        \
      mco/verbose.mco 
o/expanders.o classes/mimosa/inria/scheme2js/expanders.class:            \
      mco/pobject-conv.mco mco/expand.mco 
o/compile.o classes/mimosa/inria/scheme2js/compile.class:                \
      mco/compile-optimized-set.mco mco/compile-optimized-boolify.mco    \
      mco/compile-optimized-call.mco mco/allocate-names.mco              \
      mco/verbose.mco mco/constants.mco mco/liveness.mco mco/locals.mco  \
      mco/mark-statements.mco mco/var.mco mco/nodes.mco mco/config.mco   \
      mco/mutable-strings.mco mco/gen-js.mco mco/protobject.mco          \
      tools.sch nodes.sch protobject.sch 
o/check.o classes/mimosa/inria/scheme2js/check.class: mco/verbose.mco    \
      mco/nodes.mco mco/protobject.mco tools.sch nodes.sch               \
      protobject.sch 
o/mark-statements.o classes/mimosa/inria/scheme2js/mark-statements.class: \
      mco/verbose.mco mco/var.mco mco/config.mco mco/nodes.mco           \
      mco/protobject.mco nodes.sch protobject.sch 
o/runtime-expander.o classes/mimosa/inria/scheme2js/runtime-expander.class: \
      mco/pobject-conv.mco mco/verbose.mco mco/config.mco 
o/transform-util.o classes/mimosa/inria/scheme2js/transform-util.class:  \
      mco/var.mco mco/symbol.mco mco/nodes.mco mco/protobject.mco        \
      nodes.sch protobject.sch 
o/inline.o classes/mimosa/inria/scheme2js/inline.class: mco/verbose.mco  \
      mco/transform-util.mco mco/fun-size.mco mco/nested-closures.mco    \
      mco/use-count.mco mco/locals.mco mco/side.mco mco/var.mco          \
      mco/nodes.mco mco/config.mco mco/deep-clone.mco mco/protobject.mco \
      tools.sch nodes.sch protobject.sch 
o/locations.o classes/mimosa/inria/scheme2js/locations.class:            \
      mco/nodes.mco mco/verbose.mco mco/config.mco mco/protobject.mco    \
      tools.sch nodes.sch protobject.sch 
o/traverse.o classes/mimosa/inria/scheme2js/traverse.class:              \
      mco/verbose.mco mco/nodes.mco mco/protobject.mco nodes.sch         \
      protobject.sch 
o/compile.o classes/mimosa/inria/scheme2js/compile.class:                \
      mco/compile-optimized-set.mco mco/compile-optimized-boolify.mco    \
      mco/compile-optimized-call.mco mco/allocate-names.mco              \
      mco/verbose.mco mco/constants.mco mco/liveness.mco mco/locals.mco  \
      mco/mark-statements.mco mco/var.mco mco/nodes.mco mco/config.mco   \
      mco/mutable-strings.mco mco/gen-js.mco mco/protobject.mco          \
      tools.sch nodes.sch protobject.sch 
o/use-count.o classes/mimosa/inria/scheme2js/use-count.class:            \
      mco/verbose.mco mco/var.mco mco/nodes.mco mco/protobject.mco       \
      nodes.sch protobject.sch 
o/module.o classes/mimosa/inria/scheme2js/module.class: mco/config.mco   \
      mco/infotron.mco mco/verbose.mco 
o/nodes.o classes/mimosa/inria/scheme2js/nodes.class: mco/verbose.mco    \
      mco/protobject.mco protobject.sch 
o/captured-vars.o classes/mimosa/inria/scheme2js/captured-vars.class:    \
      mco/verbose.mco mco/side.mco mco/free-vars.mco mco/nodes.mco       \
      mco/protobject.mco tools.sch nodes.sch protobject.sch 
o/while.o classes/mimosa/inria/scheme2js/while.class: mco/verbose.mco    \
      mco/loop-updates.mco mco/tail.mco mco/symbol.mco mco/var.mco       \
      mco/nodes.mco mco/config.mco mco/mark-statements.mco               \
      mco/protobject.mco tools.sch nodes.sch protobject.sch 
o/symbol.o classes/mimosa/inria/scheme2js/symbol.class: mco/gen-js.mco   \
      mco/verbose.mco mco/var.mco mco/nodes.mco mco/config.mco           \
      mco/mapping2.mco mco/mapping1.mco mco/protobject.mco tools.sch     \
      nodes.sch protobject.sch 
o/check.o classes/mimosa/inria/scheme2js/check.class: mco/verbose.mco    \
      mco/nodes.mco mco/protobject.mco tools.sch nodes.sch               \
      protobject.sch 
o/deep-clone.o classes/mimosa/inria/scheme2js/deep-clone.class:          \
      mco/verbose.mco mco/var.mco mco/nodes.mco mco/protobject.mco       \
      tools.sch nodes.sch protobject.sch 
o/free-vars.o classes/mimosa/inria/scheme2js/free-vars.class:            \
      mco/verbose.mco mco/nodes.mco mco/locals.mco mco/protobject.mco    \
      tools.sch nodes.sch protobject.sch 
o/statements.o classes/mimosa/inria/scheme2js/statements.class:          \
      mco/verbose.mco mco/var.mco mco/nodes.mco mco/mark-statements.mco  \
      mco/protobject.mco tools.sch nodes.sch protobject.sch 
o/fun-size.o classes/mimosa/inria/scheme2js/fun-size.class:              \
      mco/verbose.mco mco/var.mco mco/nodes.mco mco/protobject.mco       \
      nodes.sch protobject.sch 
o/mapping1.o classes/mimosa/inria/scheme2js/mapping1.class: mapping.sch 
o/node-elimination.o classes/mimosa/inria/scheme2js/node-elimination.class: \
      mco/mark-statements.mco mco/verbose.mco mco/nodes.mco              \
      mco/protobject.mco nodes.sch protobject.sch 
o/scope.o classes/mimosa/inria/scheme2js/scope.class:                    \
      mco/mark-statements.mco mco/captured-vars.mco mco/free-vars.mco    \
      mco/locals.mco mco/var.mco mco/verbose.mco mco/config.mco          \
      mco/nodes.mco mco/protobject.mco tools.sch nodes.sch               \
      protobject.sch 
o/expanders.o classes/mimosa/inria/scheme2js/expanders.class:            \
      mco/pobject-conv.mco mco/expand.mco 
o/protobject.o classes/mimosa/inria/scheme2js/protobject.class:          \
      tools.sch protobject.sch 
o/infotron.o classes/mimosa/inria/scheme2js/infotron.class:              \
      mco/config.mco mco/mutable-strings.mco mco/module.mco              \
      mco/verbose.mco 
o/tail.o classes/mimosa/inria/scheme2js/tail.class: mco/verbose.mco      \
      mco/nodes.mco mco/protobject.mco nodes.sch protobject.sch 
o/compile-optimized-set.o classes/mimosa/inria/scheme2js/compile-optimized-set.class: \
      mco/verbose.mco mco/var.mco mco/nodes.mco mco/config.mco           \
      mco/protobject.mco tools.sch nodes.sch protobject.sch 
o/dot-expand.o classes/mimosa/inria/scheme2js/dot-expand.class:          \
      mco/expand.mco mco/config.mco mco/mutable-strings.mco              \
      mco/verbose.mco 
o/use-count.o classes/mimosa/inria/scheme2js/use-count.class:            \
      mco/verbose.mco mco/var.mco mco/nodes.mco mco/protobject.mco       \
      nodes.sch protobject.sch 
o/propagation.o classes/mimosa/inria/scheme2js/propagation.class:        \
      mco/verbose.mco mco/use-count.mco mco/side.mco mco/var.mco         \
      mco/nodes.mco mco/config.mco mco/free-vars.mco                     \
      mco/var-ref-util.mco mco/protobject.mco tools.sch nodes.sch        \
      protobject.sch 
o/callcc_resume_push.o classes/mimosa/inria/scheme2js/callcc_resume_push.class: \
      mco/verbose.mco mco/mark-statements.mco mco/tail.mco mco/nodes.mco \
      mco/config.mco mco/protobject.mco tools.sch nodes.sch              \
      protobject.sch 
o/constant-propagation.o classes/mimosa/inria/scheme2js/constant-propagation.class: \
      mco/verbose.mco mco/use-count.mco mco/side.mco mco/var.mco         \
      mco/nodes.mco mco/config.mco mco/var-ref-util.mco                  \
      mco/protobject.mco nodes.sch protobject.sch 
o/pobject-conv.o classes/mimosa/inria/scheme2js/pobject-conv.class:      \
      mco/verbose.mco mco/protobject.mco mco/config.mco mco/nodes.mco    \
      nodes.sch protobject.sch 
o/scope.o classes/mimosa/inria/scheme2js/scope.class:                    \
      mco/mark-statements.mco mco/captured-vars.mco mco/free-vars.mco    \
      mco/locals.mco mco/var.mco mco/verbose.mco mco/config.mco          \
      mco/nodes.mco mco/protobject.mco tools.sch nodes.sch               \
      protobject.sch 
o/inline.o classes/mimosa/inria/scheme2js/inline.class: mco/verbose.mco  \
      mco/transform-util.mco mco/fun-size.mco mco/nested-closures.mco    \
      mco/use-count.mco mco/locals.mco mco/side.mco mco/var.mco          \
      mco/nodes.mco mco/config.mco mco/deep-clone.mco mco/protobject.mco \
      tools.sch nodes.sch protobject.sch 
o/callcc_resume_push.o classes/mimosa/inria/scheme2js/callcc_resume_push.class: \
      mco/verbose.mco mco/mark-statements.mco mco/tail.mco mco/nodes.mco \
      mco/config.mco mco/protobject.mco tools.sch nodes.sch              \
      protobject.sch 
o/free-vars.o classes/mimosa/inria/scheme2js/free-vars.class:            \
      mco/verbose.mco mco/nodes.mco mco/locals.mco mco/protobject.mco    \
      tools.sch nodes.sch protobject.sch 
o/tail.o classes/mimosa/inria/scheme2js/tail.class: mco/verbose.mco      \
      mco/nodes.mco mco/protobject.mco nodes.sch protobject.sch 
o/compile-optimized-set.o classes/mimosa/inria/scheme2js/compile-optimized-set.class: \
      mco/verbose.mco mco/var.mco mco/nodes.mco mco/config.mco           \
      mco/protobject.mco tools.sch nodes.sch protobject.sch 
o/node-elimination.o classes/mimosa/inria/scheme2js/node-elimination.class: \
      mco/mark-statements.mco mco/verbose.mco mco/nodes.mco              \
      mco/protobject.mco nodes.sch protobject.sch 
o/trampoline.o classes/mimosa/inria/scheme2js/trampoline.class:          \
      mco/var-ref-util.mco mco/verbose.mco mco/side.mco mco/tail.mco     \
      mco/var.mco mco/symbol.mco mco/nodes.mco mco/config.mco            \
      mco/protobject.mco tools.sch nodes.sch protobject.sch 
o/var-elimination.o classes/mimosa/inria/scheme2js/var-elimination.class: \
      mco/verbose.mco mco/use-count.mco mco/side.mco mco/var.mco         \
      mco/nodes.mco mco/config.mco mco/var-ref-util.mco                  \
      mco/protobject.mco tools.sch nodes.sch protobject.sch 
o/verbose.o classes/mimosa/inria/scheme2js/verbose.class: mco/config.mco 
o/constant-propagation.o classes/mimosa/inria/scheme2js/constant-propagation.class: \
      mco/verbose.mco mco/use-count.mco mco/side.mco mco/var.mco         \
      mco/nodes.mco mco/config.mco mco/var-ref-util.mco                  \
      mco/protobject.mco nodes.sch protobject.sch 
o/callcc_checkpoint.o classes/mimosa/inria/scheme2js/callcc_checkpoint.class: \
      mco/verbose.mco mco/callcc_locations.mco mco/tail.mco              \
      mco/nodes.mco mco/config.mco mco/protobject.mco tools.sch          \
      nodes.sch protobject.sch 
o/rm-unused-vars.o classes/mimosa/inria/scheme2js/rm-unused-vars.class:  \
      mco/verbose.mco mco/use-count.mco mco/var.mco mco/nodes.mco        \
      mco/protobject.mco tools.sch nodes.sch protobject.sch 
o/nested-closures.o classes/mimosa/inria/scheme2js/nested-closures.class: \
      mco/verbose.mco mco/captured-vars.mco mco/free-vars.mco            \
      mco/locals.mco mco/nodes.mco mco/protobject.mco tools.sch          \
      nodes.sch protobject.sch 
o/tail-rec.o classes/mimosa/inria/scheme2js/tail-rec.class:              \
      mco/verbose.mco mco/captured-vars.mco mco/loop-updates.mco         \
      mco/tail.mco mco/side.mco mco/transform-util.mco                   \
      mco/var-ref-util.mco mco/var.mco mco/symbol.mco mco/nodes.mco      \
      mco/config.mco mco/protobject.mco tools.sch nodes.sch              \
      protobject.sch 
o/verbose.o classes/mimosa/inria/scheme2js/verbose.class: mco/config.mco 
o/main.o classes/mimosa/inria/scheme2js/main.class: version.sch          \
      mco/config.mco mco/scheme2js.mco 
o/locals.o classes/mimosa/inria/scheme2js/locals.class: mco/verbose.mco  \
      mco/nodes.mco mco/protobject.mco tools.sch nodes.sch               \
      protobject.sch 
o/encapsulation.o classes/mimosa/inria/scheme2js/encapsulation.class:    \
      mco/verbose.mco mco/config.mco mco/var.mco mco/nodes.mco           \
      mco/symbol.mco mco/protobject.mco tools.sch nodes.sch              \
      protobject.sch 
o/scheme2js.o classes/mimosa/inria/scheme2js/scheme2js.class:            \
      mco/check.mco mco/verbose.mco mco/callcc.mco mco/locations.mco     \
      mco/constants.mco mco/rm-unused-vars.mco mco/liveness.mco          \
      mco/protobject.mco mco/scope.mco mco/trampoline.mco mco/while.mco  \
      mco/var-elimination.mco mco/propagation.mco                        \
      mco/constant-propagation.mco mco/loop-hoist.mco mco/inline.mco     \
      mco/traverse.mco mco/node-elimination.mco mco/statements.mco       \
      mco/encapsulation.mco mco/tail-rec.mco mco/side.mco                \
      mco/letrec_expansion.mco mco/symbol.mco mco/dot-expand.mco         \
      mco/runtime-expander.mco mco/expanders.mco mco/expand.mco          \
      mco/pobject-conv.mco mco/compile.mco mco/infotron.mco              \
      mco/module.mco mco/config.mco version.sch 
o/var.o classes/mimosa/inria/scheme2js/var.class: mco/nodes.mco          \
      mco/protobject.mco nodes.sch protobject.sch 
o/callcc.o classes/mimosa/inria/scheme2js/callcc.class:                  \
      mco/callcc_resume_push.mco mco/callcc_locations.mco                \
      mco/callcc_checkpoint.mco mco/callcc_anormalform.mco               \
      mco/node-elimination.mco mco/verbose.mco mco/var-ref-util.mco      \
      mco/var.mco mco/mark-statements.mco mco/locals.mco mco/tail.mco    \
      mco/free-vars.mco mco/side.mco mco/nodes.mco mco/config.mco        \
      mco/protobject.mco tools.sch nodes.sch protobject.sch 
o/callcc_anormalform.o classes/mimosa/inria/scheme2js/callcc_anormalform.class: \
      mco/verbose.mco mco/var.mco mco/callcc_locations.mco mco/nodes.mco \
      mco/config.mco mco/protobject.mco tools.sch nodes.sch              \
      protobject.sch 
o/liveness.o classes/mimosa/inria/scheme2js/liveness.class:              \
      mco/verbose.mco mco/nodes.mco mco/captured-vars.mco                \
      mco/protobject.mco nodes.sch protobject.sch 
o/callcc_locations.o classes/mimosa/inria/scheme2js/callcc_locations.class: \
      mco/verbose.mco mco/var-ref-util.mco mco/var.mco mco/locals.mco    \
      mco/free-vars.mco mco/side.mco mco/nodes.mco mco/config.mco        \
      mco/protobject.mco tools.sch nodes.sch protobject.sch 
o/loop-updates.o classes/mimosa/inria/scheme2js/loop-updates.class:      \
      mco/verbose.mco mco/config.mco mco/var.mco mco/var-ref-util.mco    \
      mco/nodes.mco mco/protobject.mco tools.sch nodes.sch               \
      protobject.sch 
o/callcc_anormalform.o classes/mimosa/inria/scheme2js/callcc_anormalform.class: \
      mco/verbose.mco mco/var.mco mco/callcc_locations.mco mco/nodes.mco \
      mco/config.mco mco/protobject.mco tools.sch nodes.sch              \
      protobject.sch 
o/loop-hoist.o classes/mimosa/inria/scheme2js/loop-hoist.class:          \
      mco/verbose.mco mco/nodes.mco mco/captured-vars.mco mco/side.mco   \
      mco/config.mco mco/protobject.mco tools.sch nodes.sch              \
      protobject.sch 
o/side.o classes/mimosa/inria/scheme2js/side.class: mco/verbose.mco      \
      mco/var.mco mco/nodes.mco mco/config.mco mco/protobject.mco        \
      nodes.sch protobject.sch 
o/constants.o classes/mimosa/inria/scheme2js/constants.class:            \
      mco/verbose.mco mco/var.mco mco/mark-statements.mco mco/nodes.mco  \
      mco/config.mco mco/protobject.mco tools.sch nodes.sch              \
      protobject.sch 
o/compile-optimized-boolify.o classes/mimosa/inria/scheme2js/compile-optimized-boolify.class: \
      mco/verbose.mco mco/var.mco mco/nodes.mco mco/config.mco           \
      mco/protobject.mco tools.sch nodes.sch protobject.sch 
o/captured-vars.o classes/mimosa/inria/scheme2js/captured-vars.class:    \
      mco/verbose.mco mco/side.mco mco/free-vars.mco mco/nodes.mco       \
      mco/protobject.mco tools.sch nodes.sch protobject.sch 
o/fun-size.o classes/mimosa/inria/scheme2js/fun-size.class:              \
      mco/verbose.mco mco/var.mco mco/nodes.mco mco/protobject.mco       \
      nodes.sch protobject.sch 
o/propagation.o classes/mimosa/inria/scheme2js/propagation.class:        \
      mco/verbose.mco mco/use-count.mco mco/side.mco mco/var.mco         \
      mco/nodes.mco mco/config.mco mco/free-vars.mco                     \
      mco/var-ref-util.mco mco/protobject.mco tools.sch nodes.sch        \
      protobject.sch 
o/mapping2.o classes/mimosa/inria/scheme2js/mapping2.class: mapping.sch 
o/mark-statements.o classes/mimosa/inria/scheme2js/mark-statements.class: \
      mco/verbose.mco mco/var.mco mco/config.mco mco/nodes.mco           \
      mco/protobject.mco nodes.sch protobject.sch 
o/var-ref-util.o classes/mimosa/inria/scheme2js/var-ref-util.class:      \
      mco/nodes.mco mco/config.mco mco/protobject.mco tools.sch          \
      nodes.sch protobject.sch 
o/allocate-names.o classes/mimosa/inria/scheme2js/allocate-names.class:  \
      mco/gen-js.mco mco/verbose.mco mco/var.mco mco/nodes.mco           \
      mco/config.mco mco/free-vars.mco mco/protobject.mco tools.sch      \
      nodes.sch protobject.sch 
o/side.o classes/mimosa/inria/scheme2js/side.class: mco/verbose.mco      \
      mco/var.mco mco/nodes.mco mco/config.mco mco/protobject.mco        \
      nodes.sch protobject.sch 
o/deep-clone.o classes/mimosa/inria/scheme2js/deep-clone.class:          \
      mco/verbose.mco mco/var.mco mco/nodes.mco mco/protobject.mco       \
      tools.sch nodes.sch protobject.sch 
o/scheme2js.o classes/mimosa/inria/scheme2js/scheme2js.class:            \
      mco/check.mco mco/verbose.mco mco/callcc.mco mco/locations.mco     \
      mco/constants.mco mco/rm-unused-vars.mco mco/liveness.mco          \
      mco/protobject.mco mco/scope.mco mco/trampoline.mco mco/while.mco  \
      mco/var-elimination.mco mco/propagation.mco                        \
      mco/constant-propagation.mco mco/loop-hoist.mco mco/inline.mco     \
      mco/traverse.mco mco/node-elimination.mco mco/statements.mco       \
      mco/encapsulation.mco mco/tail-rec.mco mco/side.mco                \
      mco/letrec_expansion.mco mco/symbol.mco mco/dot-expand.mco         \
      mco/runtime-expander.mco mco/expanders.mco mco/expand.mco          \
      mco/pobject-conv.mco mco/compile.mco mco/infotron.mco              \
      mco/module.mco mco/config.mco version.sch 
o/callcc_locations.o classes/mimosa/inria/scheme2js/callcc_locations.class: \
      mco/verbose.mco mco/var-ref-util.mco mco/var.mco mco/locals.mco    \
      mco/free-vars.mco mco/side.mco mco/nodes.mco mco/config.mco        \
      mco/protobject.mco tools.sch nodes.sch protobject.sch 
o/compile-optimized-call.o classes/mimosa/inria/scheme2js/compile-optimized-call.class: \
      mco/verbose.mco mco/var.mco mco/nodes.mco mco/config.mco           \
      mco/protobject.mco tools.sch nodes.sch protobject.sch 

#bdepend stop
