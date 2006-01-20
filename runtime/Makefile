#*=====================================================================*/
#*    serrano/prgm/project/hop/runtime/Makefile                        */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Wed Apr  1 18:45:46 1998                          */
#*    Last change :  Fri Jan 20 13:56:50 2006 (serrano)                */
#*    -------------------------------------------------------------    */
#*    The makefile to build the HOP library.                           */
#*=====================================================================*/
.PHONY: do 

do: build

#*---------------------------------------------------------------------*/
#*    Standard Bigloo configuration                                    */
#*---------------------------------------------------------------------*/
include ../etc/Makefile.hopconfig
include $(BIGLOOLIBDIR)/Makefile.config

#*---------------------------------------------------------------------*/
#*    Compilers, Tools and Destinations                                */
#*---------------------------------------------------------------------*/
# the library name
TARGETNAME	= hop
# The heap file
HEAP_FILE	= $(BUILDLIBDIR)/$(TARGETNAME).heap
HEAPJVM_FILE	= $(BUILDLIBDIR)/$(TARGETNAME).jheap
# Where to store the library class files
PBASE		= bigloo.$(TARGETNAME)
CLASS_DIR	= o/class_s/bigloo/$(TARGETNAME)
CLASS_EDIR	= o/class_e/bigloo/$(TARGETNAME)
DOTNET_OBJ_DIR	= o/dotnet_s
DOTNET_OBJ_EDIR	= o/dotnet_e

#*---------------------------------------------------------------------*/
#*    Scheme extended objects                                          */
#*---------------------------------------------------------------------*/
_OBJECTS	= configure param misc types thread \
                  xml dom read \
                  read-js css js-lib cgi \
                  hop service builtin \
                  job mime user \
                  http-lib http-response http-shoutcast http-error \
                  html-extra html-notepad html-tree html-slider \
                  html-paned html-tabslider html-inline \
                  event

OBJECTS		= $(_OBJECTS:%=o/%.o)
EOBJECTS	= o/make-lib.o

BGL_CLASSES	= $(_OBJECTS:%=$(CLASS_DIR)/%.class)
BGL_ECLASSES	= $(CLASS_EDIR)/make-lib.class

BGL_DOTNET_OBJ	= $(_OBJECTS:%=$(DOTNET_OBJ_DIR)/%.obj)
BGL_DOTNET_EOBJ	= $(DOTNET_OBJ_EDIR)/make-lib.obj

SOURCES		= $(_OBJECTS:%=%.scm)

INCLUDES	= compiler-macro.sch eval-macro.sch xml.sch param.sch \
                  http-lib.sch service.sch

#*---------------------------------------------------------------------*/
#*    Sources                                                          */
#*---------------------------------------------------------------------*/
POPULATION	= $(SOURCES) $(INCLUDES) make-lib.scm Makefile

#*---------------------------------------------------------------------*/
#*    all                                                              */
#*---------------------------------------------------------------------*/
.PHONY: build native jvm dotnet

build: $(BACKEND)

native: heap-c lib-c
jvm: heap-jvm lib-jvm
dotnet: heap-jvm lib-dotnet

#*---------------------------------------------------------------------*/
#*    Suffixes                                                         */
#*---------------------------------------------------------------------*/
.SUFFIXES:
.SUFFIXES: .scm .class .java .o .cs .obj

#*---------------------------------------------------------------------*/
#*    The implicit rules                                               */
#*---------------------------------------------------------------------*/
o/%.o: %.scm
	@ $(call compile2,BIGLOO,BCFLAGS,BLFLAGS,$*.scm -o $@ -c)

$(CLASS_DIR)/%.class: %.scm
	$(BIGLOO) -jvm $(BJVMFLAGS) $(EFLAGS) $(BHOPFLAGS) -c -jvm-purify $< -o $@

$(CLASS_EDIR)/%.class: %.scm
	$(BIGLOO) -jvm $(BJVMFLAGS) $(EFLAGS) $(BHOPFLAGS) -c -jvm-purify $< -o $@

$(DOTNET_OBJ_DIR)/%.obj: %.scm
	$(BIGLOO) -dotnet \
            -dotnet-mono-workaround -dotnet-pnet-workaround \
             $(BDOTNETFLAGS) $(EFLAGS) $(BHOPFLAGS) -c -dotnet-managed $< -o $@

$(DOTNET_OBJ_EDIR)/%.obj: %.scm
	$(BIGLOO) -dotnet \
            -dotnet-mono-workaround -dotnet-pnet-workaround \
             $(BDOTNETFLAGS) $(EFLAGS) $(BHOPFLAGS) -c -dotnet-managed $< -o $@

#*---------------------------------------------------------------------*/
#*    The heap construction                                            */
#*---------------------------------------------------------------------*/
.PHONY: heap-c heap-jvm

heap-c: .afile $(HEAP_FILE)
heap-jvm: .afile .jfile $(HEAPJVM_FILE)

$(HEAP_FILE): make-lib.scm
	@ $(RM) -f $(HEAP_FILE)
	@ $(call compile2,BIGLOO,BCFLAGS,BHFLAGS,$< -addheap $@ -heap-library $(TARGETNAME))
	@ $(call done,$(COLOR),"",$@,"...done.")

$(HEAPJVM_FILE): .jfile .afile make-lib.scm
	@ $(RM) -f $(HEAPJVM_FILE)
	@ $(BIGLOO) -jvm  $(EFLAGS) $(BHEAPFLAGS) make-lib.scm -addheap $(HEAPJVM_FILE)
	@ $(call done,$(COLOR),"",$@,"...done.")

#*---------------------------------------------------------------------*/
#*    lib                                                              */
#*---------------------------------------------------------------------*/
.PHONY: lib-c lib-jvm lib-dotnet

lib: lib-jvm lib-dotnet

#*--- lib-c -----------------------------------------------------------*/
lib-c: $(TAGS) .afile o lib-c-static lib-c-shared

o:
	mkdir -p $@

lib-c-static: $(BUILDLIBDIR)/lib$(TARGETNAME)_s-$(HOPRELEASE).a \
              $(BUILDLIBDIR)/lib$(TARGETNAME)_u-$(HOPRELEASE).a

lib-c-shared:
	@ if [ "$(SHAREDLIBRARYSUPPORT)" = "yes" ]; then \
           $(MAKE) lib-c.$(SHAREDSUFFIX); \
        fi

lib-c.$(SHAREDSUFFIX): $(BUILDLIBDIR)/lib$(TARGETNAME)_s-$(HOPRELEASE).$(SHAREDSUFFIX) \
                       $(BUILDLIBDIR)/lib$(TARGETNAME)_u-$(HOPRELEASE).$(SHAREDSUFFIX) \
                       $(BUILDLIBDIR)/lib$(TARGETNAME)_e-$(HOPRELEASE).$(SHAREDSUFFIX)

$(BUILDLIBDIR)/lib$(TARGETNAME)_u-$(HOPRELEASE).$(SHAREDSUFFIX): $(BUILDLIBDIR)/lib$(TARGETNAME)_s-$(HOPRELEASE).$(SHAREDSUFFIX)
	@ cd $(BUILDLIBDIR) && \
        $(RM) -f lib$(TARGETNAME)_u-$(HOPRELEASE).$(SHAREDSUFFIX) && \
        ln -s lib$(TARGETNAME)_s-$(HOPRELEASE).$(SHAREDSUFFIX) \
              lib$(TARGETNAME)_u-$(HOPRELEASE).$(SHAREDSUFFIX)
	@ $(call done,$(COLOR),"",$@,"...done.")

$(BUILDLIBDIR)/lib$(TARGETNAME)_s-$(HOPRELEASE).$(SHAREDSUFFIX): .afile $(OBJECTS)
	@ $(MAKE) shared-lib \
             FORCELD=true \
             LIBDEST=$@ \
             SONAME=lib$(TARGETNAME)_s-$(HOPRELEASE).$(SHAREDSUFFIX) \
             CLOSELIBS="-lbigloo_s-$(HOPRELEASE)"
	@ $(call done,$(COLOR),"",$@,"...done.")

$(BUILDLIBDIR)/lib$(TARGETNAME)_e-$(HOPRELEASE).$(SHAREDSUFFIX): .afile $(EOBJECTS)
	@ $(MAKE) shared-lib \
             OBJECTS=$(EOBJECTS) \
             FORCELD=true \
             LIBDEST=$@ \
             SONAME=lib$(TARGETNAME)_e-$(HOPRELEASE).$(SHAREDSUFFIX) \
             CLOSELIBS="-lbigloo_s-$(HOPRELEASE)"
	@ $(call done,$(COLOR),"",$@,"...done.")

$(BUILDLIBDIR)/lib$(TARGETNAME)_u-$(HOPRELEASE).a: $(BUILDLIBDIR)/lib$(TARGETNAME)_s-$(HOPRELEASE).a
	@ cd $(BUILDLIBDIR) && \
        $(RM) -f lib$(TARGETNAME)_u-$(HOPRELEASE).a && \
        ln -s lib$(TARGETNAME)_s-$(HOPRELEASE).a lib$(TARGETNAME)_u-$(HOPRELEASE).a
	@ $(call done,$(COLOR),"",$@,"...done.")

$(BUILDLIBDIR)/lib$(TARGETNAME)_s-$(HOPRELEASE).a: .afile $(OBJECTS)
	@ $(RM) -f $(BUILDLIBDIR)/lib$(TARGETNAME)_s-$(HOPRELEASE).a
	@ $(AR) $(ARFLAGS) $(BUILDLIBDIR)/lib$(TARGETNAME)_s-$(HOPRELEASE).a $(OBJECTS)
	@ $(RANLIB) $(BUILDLIBDIR)/lib$(TARGETNAME)_s-$(HOPRELEASE).a
	@ $(call done,$(COLOR),"",$@,"...done.")

#*--- lib-jvm ---------------------------------------------------------*/
lib-jvm: $(TAGS) $(CLASS_DIR) $(CLASS_EDIR) lib.zip

$(CLASS_DIR): 
	mkdir -p $(CLASS_DIR)

$(CLASS_EDIR): 
	mkdir -p $(CLASS_EDIR)

lib.zip: $(BUILDLIBDIR)/bigloo$(TARGETNAME)_s-$(HOPRELEASE).zip \
         $(BUILDLIBDIR)/bigloo$(TARGETNAME)_e-$(HOPRELEASE).zip

$(BUILDLIBDIR)/bigloo$(TARGETNAME)_s-$(HOPRELEASE).zip: .afile .jfile $(BGL_CLASSES)
	@ $(RM) -f $@
	@ (cd o/class_s && \
	  $(ZIP) -q $(ZFLAGS) $@ $(CLASS_DIR:o/class_s/%=%)/*.class)
	@ echo "$@ done..."
	@ echo "-------------------------------"

$(BUILDLIBDIR)/bigloo$(TARGETNAME)_e-$(HOPRELEASE).zip: .afile .jfile $(BGL_ECLASSES)
	@ $(RM) -f $@
	@ (cd o/class_e && \
	  $(ZIP) -q $(ZFLAGS) $@ $(CLASS_EDIR:o/class_e/%=%)/*.class)
	@ echo "$@ done..."
	@ echo "-------------------------------"

#*--- lib-dotnet ---------------------------------------------------------*/
lib-dotnet: $(TAGS) $(DOTNET_OBJ_DIR) $(DOTNET_OBJ_EDIR) lib.dll

$(DOTNET_OBJ_DIR):
	mkdir -p $(DOTNET_OBJ_DIR)

$(DOTNET_OBJ_EDIR):
	mkdir -p $(DOTNET_OBJ_EDIR)

lib.dll: $(BUILDLIBDIR)/$(TARGETNAME)_u-$(HOPRELEASE).dll \
         $(BUILDLIBDIR)/$(TARGETNAME)_e-$(HOPRELEASE).dll

$(BUILDLIBDIR)/$(TARGETNAME)_u-$(HOPRELEASE).dll: .afile $(BGL_DOTNET_OBJ) $(DOTNET_OBJ_SRC)
	@ $(RM) -f $(BUILDLIBDIR)/$(TARGETNAME)_s-$(HOPRELEASE).dll
	@ $(RM) -f $(BUILDLIBDIR)/$(TARGETNAME)_u-$(HOPRELEASE).dll
	@ (cd $(DOTNET_OBJ_DIR) && \
	  $(DOTNETCSCC) \
		$(DOTNETFTDLLPATH) $(DOTNETLINKBIGLOODLL) \
                -shared -o $(BUILDLIBDIR)/$(TARGETNAME)_s-$(HOPRELEASE).dll \
                $(DOTNET_OBJ_SRC:$(DOTNET_OBJ_DIR)/%=%) \
		$(BGL_DOTNET_OBJ:$(DOTNET_OBJ_DIR)/%=%))
	@ echo "$@ done..."
	 (cd o/dotnet_s && \
	  $(DOTNETCSCC) \
		$(DOTNETFTDLLPATH) $(DOTNETLINKBIGLOODLL) \
                -shared -o $(BUILDLIBDIR)/$(TARGETNAME)_u-$(HOPRELEASE).dll \
                $(DOTNET_OBJ_SRC:$(DOTNET_OBJ_DIR)/%=%) \
		$(BGL_DOTNET_OBJ:$(DOTNET_OBJ_DIR)/%=%))
	@ echo "$@ done..."
	@ echo "-------------------------------"

$(BUILDLIBDIR)/$(TARGETNAME)_e-$(HOPRELEASE).dll: .afile $(BGL_DOTNET_EOBJ)
	@ $(RM) -f $(BUILDLIBDIR)/$(TARGETNAME)_e-$(HOPRELEASE).dll
	@ (cd $(DOTNET_OBJ_EDIR) && \
	  $(DOTNETCSCC) \
		$(DOTNETFTDLLPATH) $(DOTNETLINKBIGLOODLL) \
                 -shared -o $(BUILDLIBDIR)/$(TARGETNAME)_e-$(HOPRELEASE).dll \
                            $(BUILDLIBDIR)/$(TARGETNAME)_u-$(HOPRELEASE).dll \
		$(BGL_DOTNET_EOBJ:$(DOTNET_OBJ_EDIR)/%=%))
	@ echo "$@ done..."
	@ echo "-------------------------------"

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
                   -pbase $(PBASE) $(_BGL_OBJECTS:%=%.scm) make-lib.scm

.etags: $(SOURCES)
	@ $(BTAGS) -o .etags $(SOURCES)

#*---------------------------------------------------------------------*/
#*    dep                                                              */
#*---------------------------------------------------------------------*/
.PHONY: dep
dep: 
	@(num=`grep -n '^#bdepend start' Makefile | awk -F: '{ print $$1}' -`;\
          head -n `expr $$num - 1` Makefile > /tmp/Makefile.aux)
	@ $(DEPEND) -fno-mco \
                    -search-path . \
                    -strict-obj-dir o \
                    $(SOURCES) >> /tmp/Makefile.aux
	@ mv /tmp/Makefile.aux Makefile

#*---------------------------------------------------------------------*/
#*    stdclean                                                         */
#*---------------------------------------------------------------------*/
stdclean:
	$(RM) -f $(OBJECTS) $(_BGL_OBJECTS:%=%.c)
	$(RM) -f $(BUILDLIBDIR)/lib$(TARGETNAME)_s-$(HOPRELEASE).a
	$(RM) -f $(BUILDLIBDIR)/lib$(TARGETNAME)_u-$(HOPRELEASE).a
	$(RM) -f $(BUILDLIBDIR)/lib$(TARGETNAME)_e-$(HOPRELEASE).a
	$(RM) -f $(BUILDLIBDIR)/lib$(TARGETNAME)_s-$(HOPRELEASE).$(SHAREDSUFFIX)
	$(RM) -f $(BUILDLIBDIR)/lib$(TARGETNAME)_u-$(HOPRELEASE).$(SHAREDSUFFIX)
	$(RM) -f $(BUILDLIBDIR)/lib$(TARGETNAME)_e-$(HOPRELEASE).$(SHAREDSUFFIX)
	$(RM) -f $(BUILDLIBDIR)/bigloo$(TARGETNAME)_s-$(HOPRELEASE).zip
	$(RM) -f $(BUILDLIBDIR)/bigloo$(TARGETNAME)_u-$(HOPRELEASE).zip
	$(RM) -f $(BUILDLIBDIR)/bigloo$(TARGETNAME)_e-$(HOPRELEASE).zip
	$(RM) -f $(BUILDLIBDIR)/$(TARGETNAME)_s-$(HOPRELEASE).dll
	$(RM) -f $(BUILDLIBDIR)/$(TARGETNAME)_u-$(HOPRELEASE).dll
	$(RM) -f $(BUILDLIBDIR)/$(TARGETNAME)_e-$(HOPRELEASE).dll
	$(RM) -f $(BUILDLIBDIR)/$(TARGETNAME).jheap 
	$(RM) -f $(BUILDLIBDIR)/$(TARGETNAME).heap
	$(RM) -rf $(CLASS_DIR)
	$(RM) -rf $(DOTNET_OBJ_DIR)
	$(RM) -f *~
	$(RM) -f *.mco
	$(RM) -f *.ast

udeclean:
	$(RM) -f .afile .etags .jfile

cleanall: stdclean

devclean: cleanall

#*---------------------------------------------------------------------*/
#*    pop ...                                                          */
#*---------------------------------------------------------------------*/
pop:
	@ echo $(POPULATION:%=runtime/%)

#*---------------------------------------------------------------------*/
#*    clean                                                            */
#*---------------------------------------------------------------------*/
clean: stdclean
	$(RM) -rf o

cleanall: stdclean udeclean

distclean: cleanall

#*---------------------------------------------------------------------*/
#*    Installation                                                     */
#*---------------------------------------------------------------------*/
.PHONY: install install-c install-jvm uninstall

include $(BIGLOOLIBDIR)/Makefile.misc

install: install-$(BACKEND)

install-native:
	cp $(BUILDLIBDIR)/$(TARGETNAME).heap $(DESTDIR)$(HOPFILDIR)/$(TARGETNAME).heap && \
        chmod $(BMASK) $(DESTDIR)$(HOPFILDIR)/$(TARGETNAME).heap && \
        $(MAKE) install-lib \
                BOOTLIBDIR=$(BUILDLIBDIR) \
                FILDIR=$(HOPFILDIR)\
                LIBDIR=$(HOPLIBDIR)\
                LIB=lib$(TARGETNAME)_s-$(HOPRELEASE) && \
        $(MAKE) install-shared-lib \
                BOOTLIBDIR=$(BUILDLIBDIR) \
                FILDIR=$(HOPFILDIR) \
                LIBDIR=$(HOPLIBDIR) \
                LIB=lib$(TARGETNAME)_e-$(HOPRELEASE) && \
        $(MAKE) install-lnlib \
                BOOTLIBDIR=$(BUILDLIBDIR) \
                FILDIR=$(HOPFILDIR) \
                LIBDIR=$(HOPLIBDIR) \
                LIB=lib$(TARGETNAME)_s-$(HOPRELEASE) \
                LN=lib$(TARGETNAME)_u-$(HOPRELEASE)

install-jvm:
	cp $(BUILDLIBDIR)/$(TARGETNAME).jheap $(DESTDIR)$(HOPFILDIR)/$(TARGETNAME).jheap && \
        chmod $(BMASK) $(DESTDIR)$(HOPFILDIR)/$(TARGETNAME).jheap && \
        cp $(BUILDLIBDIR)/bigloo$(TARGETNAME)_s-$(HOPRELEASE).zip $(DESTDIR)$(HOPFILDIR) && \
        chmod $(BMASK) $(DESTDIR)$(HOPFILDIR)/bigloo$(TARGETNAME)_s-$(HOPRELEASE).zip && \
        cp $(BUILDLIBDIR)/bigloo$(TARGETNAME)_e-$(HOPRELEASE).zip $(DESTDIR)$(HOPFILDIR) && \
        chmod $(BMASK) $(DESTDIR)$(HOPFILDIR)/bigloo$(TARGETNAME)_e-$(HOPRELEASE).zip; \
        (cd $(DESTDIR)$(HOPFILDIR) && \
           $(RM) -f bigloo$(TARGETNAME)_u-$(HOPRELEASE).zip && \
           ln bigloo$(TARGETNAME)_s-$(HOPRELEASE).zip bigloo$(TARGETNAME)_u-$(HOPRELEASE).zip)

install-dotnet:
	cp $(BUILDLIBDIR)/$(TARGETNAME).jheap $(DESTDIR)$(HOPFILDIR)/$(TARGETNAME).jheap && \
        chmod $(BMASK) $(DESTDIR)$(HOPFILDIR)/$(TARGETNAME).jheap && \
        cp $(BUILDLIBDIR)/bigloo$(TARGETNAME)_s-$(HOPRELEASE).dll $(DESTDIR)$(HOPFILDIR)/bigloo$(TARGETNAME)_s-$(HOPRELEASE).dll && \
        chmod $(BMASK) $(DESTDIR)$(HOPFILDIR)/bigloo$(TARGETNAME)_s-$(HOPRELEASE).dll && \
        cp $(BUILDLIBDIR)/bigloo$(TARGETNAME)_e-$(HOPRELEASE).dll $(DESTDIR)$(HOPFILDIR)/bigloo$(TARGETNAME)_e-$(HOPRELEASE).dll && \
        chmod $(BMASK) $(DESTDIR)$(HOPFILDIR)/bigloo$(TARGETNAME)_e-$(HOPRELEASE).dll && \
        cp $(BUILDLIBDIR)/bigloo$(TARGETNAME)_u-$(HOPRELEASE).dll $(DESTDIR)$(HOPFILDIR)/bigloo$(TARGETNAME)_u-$(HOPRELEASE).dll && \
        chmod $(BMASK) $(DESTDIR)$(HOPFILDIR)/bigloo$(TARGETNAME)_u-$(HOPRELEASE).dll 

uninstall:
	-$(RM) -f $(DESTDIR)$(HOPFILDIR)/$(TARGETNAME).heap
	-$(MAKE) uninstall-lib LIB=lib$(TARGETNAME)_s-$(HOPRELEASE)
	-$(MAKE) uninstall-lib LIB=lib$(TARGETNAME)_u-$(HOPRELEASE)
	-$(MAKE) uninstall-lib LIB=lib$(TARGETNAME)_e-$(HOPRELEASE)
	-$(RM) -f $(DESTDIR)$(HOPFILDIR)/$(TARGETNAME).jheap
	-$(RM) -f $(DESTDIR)$(HOPFILDIR)/bigloo$(TARGETNAME)_s-$(HOPRELEASE).zip
	-$(RM) -f $(DESTDIR)$(HOPFILDIR)/bigloo$(TARGETNAME)_u-$(HOPRELEASE).zip
	-$(RM) -f $(DESTDIR)$(HOPFILDIR)/bigloo$(TARGETNAME)_e-$(HOPRELEASE).zip
	-$(RM) -f $(DESTDIR)$(HOPFILDIR)/bigloo$(TARGETNAME)_s-$(HOPRELEASE).dll
	-$(RM) -f $(DESTDIR)$(HOPFILDIR)/bigloo$(TARGETNAME)_u-$(HOPRELEASE).dll
	-$(RM) -f $(DESTDIR)$(HOPFILDIR)/bigloo$(TARGETNAME)_e-$(HOPRELEASE).dll

#bdepend start (don't edit)
#*---------------------------------------------------------------------*/
#*    Dependencies ...                                                 */
#*---------------------------------------------------------------------*/
o/param.o param.class: ./eval-macro.sch ./param.sch 
o/html-slider.o html-slider.class: ./compiler-macro.sch ./xml.sch 
o/html-paned.o html-paned.class: ./compiler-macro.sch ./xml.sch 
o/http-response.o ./http-response.class: ./http-lib.sch 
o/param.o ./param.class: ./eval-macro.sch ./param.sch 
o/html-extra.o html-extra.class: ./compiler-macro.sch ./xml.sch 
o/event.o event.class: ./compiler-macro.sch ./xml.sch 
o/html-tree.o html-tree.class: ./compiler-macro.sch ./xml.sch 
o/http-response.o http-response.class: ./http-lib.sch 
o/http-lib.o ./http-lib.class: ./http-lib.sch 
o/service.o ./service.class: ./eval-macro.sch ./service.sch 
o/html-notepad.o html-notepad.class: ./compiler-macro.sch ./xml.sch 
o/service.o service.class: ./eval-macro.sch ./service.sch 
o/xml.o xml.class: ./eval-macro.sch ./xml.sch 
o/html-extra.o ./html-extra.class: ./compiler-macro.sch ./xml.sch 
o/builtin.o ./builtin.class: ./compiler-macro.sch ./service.sch 
o/html-tabslider.o html-tabslider.class: ./compiler-macro.sch ./xml.sch 
o/builtin.o builtin.class: ./compiler-macro.sch ./service.sch 
o/http-lib.o http-lib.class: ./http-lib.sch 
o/xml.o ./xml.class: ./eval-macro.sch ./xml.sch 
o/http-shoutcast.o http-shoutcast.class: ./http-lib.sch 
o/html-inline.o html-inline.class: ./compiler-macro.sch ./xml.sch 

#bdepend stop
