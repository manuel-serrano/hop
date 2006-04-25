#*=====================================================================*/
#*    serrano/prgm/project/hop/scheme2js/Makefile                      */
#*    -------------------------------------------------------------    */
#*    Author      :  Florian Loitsch                                   */
#*    Creation    :  Wed Mar 15 07:27:50 2006                          */
#*    Last change :  Wed Mar 22 16:17:31 2006 (serrano)                */
#*    Copyright   :  2006 Manuel Serrano                               */
#*    -------------------------------------------------------------    */
#*    The Makefile to build the Scheme2JS library.                     */
#*=====================================================================*/
.PHONY: do 

do: build

#*---------------------------------------------------------------------*/
#*    Standard Bigloo configuration                                    */
#*---------------------------------------------------------------------*/
include ../etc/Makefile.hopconfig
include ../etc/Makefile.version
include $(BIGLOOLIBDIR)/Makefile.config

#*---------------------------------------------------------------------*/
#*    Compilers, Tools and Destinations                                */
#*---------------------------------------------------------------------*/
# the library name
TARGETNAME	= scheme2js

#*---------------------------------------------------------------------*/
#*    Scheme extended objects                                          */
#*---------------------------------------------------------------------*/
_BGL_OBJECTS	= capture collect-vars compile constants expanders \
	gen-code gen-js inline js-interface liveness node-elimination \
	nodes pobject-conv protobject scheme2js side statements symbol \
	tail-rec tail transform-util traverse var expand verbose \
	dot-expand allocate-names config

_OBJECTS	= $(_BGL_OBJECTS)

OBJECTS		= $(_OBJECTS:%=o/%.o)
EOBJECTS	= o/make-lib.o

BGL_CLASSES	= $(_OBJECTS:%=$(CLASS_DIR)/%.class)
BGL_ECLASSES	= $(CLASS_EDIR)/make-lib.class

BGL_DOTNET_OBJ	= $(_OBJECTS:%=$(DOTNET_OBJ_DIR)/%.obj)
BGL_DOTNET_EOBJ	= $(DOTNET_OBJ_EDIR)/make-lib.obj

SOURCES		= $(_OBJECTS:%=%.scm)

INCLUDES	= nodes.sch protobject.sch tools.sch \
	runtime_mapping_hack.sch compile-optimized-boolify.scm \
	compile-optimized-call.scm

#*---------------------------------------------------------------------*/
#*    Sources                                                          */
#*---------------------------------------------------------------------*/
POPULATION	= $(SOURCES) $(INCLUDES) make-lib.scm Makefile \
                  runtime/exporter.scm runtime/runtime.js

include ../etc/Makefile.library

#*---------------------------------------------------------------------*/
#*    all                                                              */
#*---------------------------------------------------------------------*/
.PHONY: build native jvm dotnet

build: $(BACKEND) runtime-build

native: heap-c lib-c
jvm: heap-jvm lib-jvm
dotnet: heap-jvm lib-dotnet

clean: runtime-clean
install: runtime-install
uninstall: runtime-uninstall

#*---------------------------------------------------------------------*/
#*    scheme2js runtime                                                */
#*---------------------------------------------------------------------*/

SCHEME2JS_RUNTIME = runtime

runtime-build: $(SCHEME2JS_RUNTIME)/runtime-interface.js $(SCHEME2JS_RUNTIME)/runtime_mapping.alist

$(SCHEME2JS_RUNTIME)/exporter: $(SCHEME2JS_RUNTIME)/exporter.scm
	$(BIGLOO) -o $@ $^

$(SCHEME2JS_RUNTIME)/runtime_mapping.alist: $(SCHEME2JS_RUNTIME)/runtime.js $(SCHEME2JS_RUNTIME)/exporter
	$(SCHEME2JS_RUNTIME)/exporter $< mapping > $@

$(SCHEME2JS_RUNTIME)/runtime-interface.js: $(SCHEME2JS_RUNTIME)/runtime.js $(SCHEME2JS_RUNTIME)/exporter
	$(SCHEME2JS_RUNTIME)/exporter $< interface > $@

runtime-clean:
	/bin/rm -f $(SCHEME2JS_RUNTIME)/exporter
	/bin/rm -f $(SCHEME2JS_RUNTIME)/runtime_mapping.alist
	/bin/rm -f $(SCHEME2JS_RUNTIME)/runtime-interface.js
	/bin/rm -f $(SCHEME2JS_RUNTIME)/*.o

runtime-install:
	mkdir -p $(HOPSHAREDIR) && \
	install -m a+r $(SCHEME2JS_RUNTIME)/runtime.js $(HOPSHAREDIR) && \
	install -m a+r $(SCHEME2JS_RUNTIME)/runtime-interface.js $(HOPSHAREDIR)

runtime-uninstall:
	/bin/rm -f $(HOPSHAREDIR)/runtime.js && \
	/bin/rm -f $(HOPSHAREDIR)/runtime-interface.js

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
                   -I $(MISC_SRC_DIR) \
                   -pbase $(PBASE) $(SOURCES)  make-lib.scm

.etags: $(SOURCES)
	@ $(BTAGS) -o .etags $(SOURCES)

#*---------------------------------------------------------------------*/
#*    Explicit dependency                                              */
#*---------------------------------------------------------------------*/
scheme2js.scm: $(SCHEME2JS_RUNTIME)/runtime_mapping.alist

#bdepend start (don't edit)
#*---------------------------------------------------------------------*/
#*    Dependencies ...                                                 */
#*---------------------------------------------------------------------*/
o/var.o ./var.class: ./nodes.sch ./protobject.sch 
o/statements.o ./statements.class: ./nodes.sch ./protobject.sch 
o/constants.o constants.class: ./tools.sch ./nodes.sch ./protobject.sch 
o/protobject.o protobject.class: ./protobject.sch 
o/traverse.o ./traverse.class: ./nodes.sch ./protobject.sch 
o/tail-rec.o tail-rec.class: ./nodes.sch ./protobject.sch 
o/transform-util.o transform-util.class: ./nodes.sch ./protobject.sch 
o/liveness.o liveness.class: ./nodes.sch ./protobject.sch 
o/collect-vars.o collect-vars.class: ./tools.sch ./nodes.sch             \
      ./protobject.sch 
o/symbol.o ./symbol.class: ./nodes.sch ./protobject.sch 
o/pobject-conv.o pobject-conv.class: ./protobject.sch 
o/gen-code.o ./gen-code.class: ./tools.sch 
o/nodes.o nodes.class: ./protobject.sch 
o/compile.o ./compile.class: ./compile-optimized-boolify.scm             \
      ./compile-optimized-call.scm ./tools.sch ./nodes.sch               \
      ./protobject.sch 
o/traverse.o traverse.class: ./nodes.sch ./protobject.sch 
o/transform-util.o ./transform-util.class: ./nodes.sch ./protobject.sch 
o/inline.o ./inline.class: ./nodes.sch ./protobject.sch 
o/compile.o compile.class: ./compile-optimized-boolify.scm               \
      ./compile-optimized-call.scm ./tools.sch ./nodes.sch               \
      ./protobject.sch 
o/nodes.o ./nodes.class: ./protobject.sch 
o/symbol.o symbol.class: ./nodes.sch ./protobject.sch 
o/statements.o statements.class: ./nodes.sch ./protobject.sch 
o/node-elimination.o node-elimination.class: ./nodes.sch                 \
      ./protobject.sch 
o/js-interface.o js-interface.class: ./runtime/runtime_mapping.alist     \
      ./runtime_mapping_hack.sch 
o/protobject.o ./protobject.class: ./protobject.sch 
o/capture.o ./capture.class: ./tools.sch ./nodes.sch ./protobject.sch 
o/tail.o ./tail.class: ./nodes.sch ./protobject.sch 
o/collect-vars.o ./collect-vars.class: ./tools.sch ./nodes.sch           \
      ./protobject.sch 
o/capture.o capture.class: ./tools.sch ./nodes.sch ./protobject.sch 
o/pobject-conv.o ./pobject-conv.class: ./protobject.sch 
o/inline.o inline.class: ./nodes.sch ./protobject.sch 
o/tail.o tail.class: ./nodes.sch ./protobject.sch 
o/gen-code.o gen-code.class: ./tools.sch 
o/node-elimination.o ./node-elimination.class: ./nodes.sch               \
      ./protobject.sch 
o/tail-rec.o ./tail-rec.class: ./nodes.sch ./protobject.sch 
o/var.o var.class: ./nodes.sch ./protobject.sch 
o/liveness.o ./liveness.class: ./nodes.sch ./protobject.sch 
o/constants.o ./constants.class: ./tools.sch ./nodes.sch                 \
      ./protobject.sch 
o/side.o ./side.class: ./nodes.sch ./protobject.sch 
o/js-interface.o ./js-interface.class: ./runtime/runtime_mapping.alist   \
      ./runtime_mapping_hack.sch 
o/side.o side.class: ./nodes.sch ./protobject.sch 

#bdepend stop
