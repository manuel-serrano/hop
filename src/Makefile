#*=====================================================================*/
#*    serrano/prgm/project/hop/hop/src/Makefile                        */
#*    -------------------------------------------------------------    */
#*    Author      :  serrano                                           */
#*    Creation    :  Fri Feb 18 17:48:08 2000                          */
#*    Last change :  Tue May  7 07:33:40 2024 (serrano)                */
#*    -------------------------------------------------------------    */
#*    The Makefile that builds Hop                                     */
#*=====================================================================*/
do: build

#*---------------------------------------------------------------------*/
#*    Configuration                                                    */
#*---------------------------------------------------------------------*/
-include ../etc/Makefile.hopconfig
-include ../etc/Makefile.version

#*---------------------------------------------------------------------*/
#*    Target and Project                                               */
#*---------------------------------------------------------------------*/
HOP_EXEC	= hop
HOPJS_EXEC	= hopjs

#*---------------------------------------------------------------------*/
#*    Sources    					               */
#*---------------------------------------------------------------------*/
MODULES		= hop_param parseargs main init

INCLUDES	= 

CLASS_PATH	= o/class_s
CLASS_DIR	= $(CLASS_PATH)/bigloo/hop

OBJECTS 	= $(MODULES:%=o/%.o)
BGL_CLASSES	= $(MODULES:%=$(CLASS_DIR)/%.class)

SOURCES 	= $(MODULES:%=%.scm)

COLOR		= 32

#*---------------------------------------------------------------------*/
#*    Population                                                       */
#*---------------------------------------------------------------------*/
POPULATION 	= $(INCLUDES) $(SOURCES) Makefile

#*---------------------------------------------------------------------*/
#*    The hop executable                                               */
#*---------------------------------------------------------------------*/
.PHONY: build dobuild link.static link.dynamic link.library

build:  o $(BACKEND)

native: $(BUILDBINDIR)/$(HOP_EXEC) $(BUILDBINDIR)/$(HOPJS_EXEC)

o: 
	mkdir -p $@

$(CLASS_DIR):
	mkdir -p $@

$(BUILDBINDIR)/$(HOP_EXEC): .afile .etags $(OBJECTS)
	$(MAKE) link.$(LINK) DEST=$@

$(BUILDBINDIR)/$(HOPJS_EXEC): .afile .etags o/hopjs.o
	$(MAKE) link.$(LINK) DEST=$@ OBJECTS=o/hopjs.o

link.dynamic:
	@ $(call link,$(BIGLOO),$(BCFLAGS) $(BLDFLAGS),$(BCFLAGSDEV),$(OBJECTS),-o,$(DEST))

link.static-bigloo:
	@ $(call link,$(BIGLOO),$(BCFLAGS),$(BCFLAGSDEV),-static-all-bigloo $(OBJECTS),-o,$(DEST))

link.static:
	@ $(call link,$(BIGLOO),$(BCFLAGS),$(BCFLAGSDEV),-static-all-bigloo -ldopt -static $(OBJECTS),-o,$(DEST))

link.library:
	echo "***ERROR: link.library not currently supported!"
	exit 1

#*---------------------------------------------------------------------*/
#*    SUFFIXES                                                         */
#*---------------------------------------------------------------------*/
.SUFFIXES:
.SUFFIXES: .scm .o

#*---------------------------------------------------------------------*/
#*    Implicit rules                                                   */
#*---------------------------------------------------------------------*/
o/%.o: %.scm .afile
	@ $(call compile,$(BIGLOO),$(BCFLAGS),-c,$< -o $@)

o/main.o: main.scm
	@ $(call compile2,$(BIGLOO),$(BCFLAGS),$(BCFLAGSDEV),-c,$< -o $@)

o/hopjs.o: hopjs.scm
	@ $(call compile2,$(BIGLOO),$(BCFLAGS),$(BCFLAGSDEV),-c,$< -o $@)

#*---------------------------------------------------------------------*/
#*    ude                                                              */
#*---------------------------------------------------------------------*/
.PHONY: ude
ude:
	@ $(MAKE) -f Makefile .afile .etags dep

.afile: $(SOURCES)
	@ $(AFILE) -o .afile $(SOURCES) 

.etags: .afile
	@ $(BTAGS) -o .etags $(SOURCES)

getsources:
	@ echo $(SOURCES)

#*---------------------------------------------------------------------*/
#*    dep                                                              */
#*---------------------------------------------------------------------*/
.PHONY: dep
dep: 
	@(num=`grep -n '^#bdepend start' Makefile | awk -F: '{ print $$1}' -`;\
          if [ "$$num " != " " ]; then \
            head -n `expr $$num - 1` Makefile > /tmp/Makefile.aux; \
          else \
            cp Makefile /tmp/Makefile.aux; \
          fi; \
          head -n `expr $$num - 1` Makefile > /tmp/Makefile.aux)
	@ $(DEPEND) -strict-obj-dir o -fno-mco $(SOURCES) >> /tmp/Makefile.aux
	@ mv /tmp/Makefile.aux Makefile

#*---------------------------------------------------------------------*/
#*    install                                                          */
#*---------------------------------------------------------------------*/
install: install-$(BACKEND)

install-native:
	$(INSTALL) $(BUILDBINDIR)/$(HOP_EXEC) $(DESTDIR)$(HOPBINDIR)/hop-$(HOPRELEASE)
	chmod $(MODEXE) $(DESTDIR)$(HOPBINDIR)/hop-$(HOPRELEASE)
	(cd $(DESTDIR)$(HOPBINDIR) && $(RM) -f hop && ln hop-$(HOPRELEASE) hop)
	$(INSTALL) $(BUILDBINDIR)/$(HOPJS_EXEC) $(DESTDIR)$(HOPBINDIR)/hopjs-$(HOPRELEASE)
	chmod $(MODEXE) $(DESTDIR)$(HOPBINDIR)/hopjs-$(HOPRELEASE)
	(cd $(DESTDIR)$(HOPBINDIR) && $(RM) -f hopjs && ln hopjs-$(HOPRELEASE) hopjs)

install-jvm:
	$(INSTALL) $(BUILDBINDIR)/$(HOP_EXEC).jar $(DESTDIR)$(HOPBINDIR)
	chmod $(MODFILE) $(DESTDIR)$(HOPBINDIR)/$(HOP_EXEC).jar

#*---------------------------------------------------------------------*/
#*    uninstall                                                        */
#*---------------------------------------------------------------------*/
uninstall:
	$(RM) -f $(DESTDIR)$(HOPBINDIR)/$(HOP_EXEC)-$(HOPRELEASE)
	$(RM) -f $(DESTDIR)$(HOPBINDIR)/$(HOP_EXEC)
	$(RM) -f $(DESTDIR)$(HOPBINDIR)/$(HOPJS_EXEC)-$(HOPRELEASE)
	$(RM) -f $(DESTDIR)$(HOPBINDIR)/$(HOPJS_EXEC)
	$(RM) -f $(DESTDIR)$(HOPBINDIR)/$(HOP_EXEC).jar

#*---------------------------------------------------------------------*/
#*    clean                                                            */
#*---------------------------------------------------------------------*/
clean:
	$(RM) -f .afile
	$(RM) -f .jfile
	$(RM) -f $(OBJECTS)
	$(RM) -f $(MODULES:%=%.c)
	$(RM) -rf o
	$(RM) -f $(BUILDBINDIR)/$(HOP_EXEC)
	$(RM) -f $(BUILDBINDIR)/$(HOPJS_EXEC)
	$(RM) -f $(BUILDBINDIR)/$(HOP_EXEC).jar
	$(RM) -f META-INF/MANIFEST.MF

devclean: clean

distclean: clean

cleanall: devclean

#*---------------------------------------------------------------------*/
#*    Dependencies                                                     */
#*---------------------------------------------------------------------*/
$(OBJECTS): .afile

#bdepend start (don't edit)
#*---------------------------------------------------------------------*/
#*    Dependencies ...                                                 */
#*---------------------------------------------------------------------*/
o/pipeline.o pipeline.class: stage.sch 
o/init.o ./init.class: libraries.sch 
o/pipeline.o ./pipeline.class: stage.sch 
o/main.o main.class: libraries.sch 
o/nothread_scheduler.o ./nothread_scheduler.class: stage.sch 
o/accept.o ./accept.class: stage.sch 
o/init.o init.class: libraries.sch 
o/nothread_scheduler.o nothread_scheduler.class: stage.sch 
o/accept.o accept.class: stage.sch 

#bdepend stop
