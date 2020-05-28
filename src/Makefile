#*=====================================================================*/
#*    serrano/prgm/project/hop/hop/src/Makefile                        */
#*    -------------------------------------------------------------    */
#*    Author      :  serrano                                           */
#*    Creation    :  Fri Feb 18 17:48:08 2000                          */
#*    Last change :  Mon May 18 07:51:43 2020 (serrano)                */
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
EXEC		= hop

#*---------------------------------------------------------------------*/
#*    Sources    					               */
#*---------------------------------------------------------------------*/
MODULES		= hop_param parseargs main init \
                  scheduler accept pipeline \
                  nothread_scheduler \
                  queue_scheduler \
                  oto_scheduler \
                  pool_scheduler \
                  amany_scheduler

INCLUDES	= stage.sch

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

native: $(BUILDBINDIR)/$(EXEC)

o: 
	mkdir -p $@

$(CLASS_DIR):
	mkdir -p $@

$(BUILDBINDIR)/$(EXEC): .afile .etags $(OBJECTS)
	$(MAKE) link.$(LINK) DEST=$@

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
	$(INSTALL) $(BUILDBINDIR)/$(EXEC) $(DESTDIR)$(HOPBINDIR)/hop-$(HOPRELEASE)
	chmod $(MODEXE) $(DESTDIR)$(HOPBINDIR)/hop-$(HOPRELEASE)
	(cd $(DESTDIR)$(HOPBINDIR) && $(RM) -f hop && ln hop-$(HOPRELEASE) hop)

install-jvm:
	$(INSTALL) $(BUILDBINDIR)/$(EXEC).jar $(DESTDIR)$(HOPBINDIR)
	chmod $(MODFILE) $(DESTDIR)$(HOPBINDIR)/$(EXEC).jar

#*---------------------------------------------------------------------*/
#*    uninstall                                                        */
#*---------------------------------------------------------------------*/
uninstall:
	$(RM) -f $(DESTDIR)$(HOPBINDIR)/$(EXEC)-$(HOPRELEASE)
	$(RM) -f $(DESTDIR)$(HOPBINDIR)/$(EXEC)
	$(RM) -f $(DESTDIR)$(HOPBINDIR)/$(EXEC).jar

#*---------------------------------------------------------------------*/
#*    clean                                                            */
#*---------------------------------------------------------------------*/
clean:
	$(RM) -f .afile
	$(RM) -f .jfile
	$(RM) -f $(OBJECTS)
	$(RM) -f $(MODULES:%=%.c)
	$(RM) -rf o
	$(RM) -f $(BUILDBINDIR)/$(EXEC)
	$(RM) -f $(BUILDBINDIR)/$(EXEC).jar
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
